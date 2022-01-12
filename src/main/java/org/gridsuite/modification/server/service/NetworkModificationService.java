/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.*;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.Branch.Side;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import com.powsybl.sld.iidm.extensions.BranchStatusAdder;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.GroovyScriptModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationService {
    private final NetworkStoreService networkStoreService;

    private final NetworkModificationRepository modificationRepository;

    private final EquipmentInfosService equipmentInfosService;

    private RestTemplate reportServerRest;

    private final ObjectMapper objectMapper;

    private static final String REPORT_API_VERSION = "v1";
    private static final String DELIMITER = "/";

    private static final String CANCEL_CATEGORY_BROKER_OUTPUT = NetworkModificationService.class.getName() + ".output-broker-messages.cancel";
    private static final Logger CANCEL_MESSAGE_LOGGER = LoggerFactory.getLogger(CANCEL_CATEGORY_BROKER_OUTPUT);

    private static final String RUN_CATEGORY_BROKER_OUTPUT = NetworkModificationService.class.getName() + ".output-broker-messages.run";
    private static final Logger RUN_MESSAGE_LOGGER = LoggerFactory.getLogger(RUN_CATEGORY_BROKER_OUTPUT);

    private static final String NETWORK_MODIFICATION_REPORT_KEY = "NetworkModification";
    private static final String NETWORK_MODIFICATION_REPORT_NAME = "Network modification";

    private static final String NETWORK_BUILD_REPORT_KEY = "NetworkBuilding";
    private static final String NETWORK_BUILD_REPORT_NAME = "Network building";

    private static final String LINE_STATUS_INFO_REPORT_SEVERITY = "LINE_STATUS_INFO";

    @Autowired
    private StreamBridge publisher;

    public NetworkModificationService(@Value("${backing-services.report-server.base-uri:http://report-server}") String reportServerURI,
                                      NetworkStoreService networkStoreService, NetworkModificationRepository modificationRepository,
                                      EquipmentInfosService equipmentInfosService) {
        this.networkStoreService = networkStoreService;
        this.modificationRepository = modificationRepository;
        this.equipmentInfosService = equipmentInfosService;

        RestTemplateBuilder restTemplateBuilder = new RestTemplateBuilder();
        reportServerRest = restTemplateBuilder.build();
        reportServerRest.setUriTemplateHandler(new DefaultUriBuilderFactory(reportServerURI));

        objectMapper = Jackson2ObjectMapperBuilder.json().build();
        objectMapper.registerModule(new ReporterModelJsonModule());
        objectMapper.setInjectableValues(new InjectableValues.Std().addValue(ReporterModelDeserializer.DICTIONARY_VALUE_ID, null));
    }

    private List<ModificationInfos> execApplyGroovyScript(NetworkStoreListener listener,
                                                          String groovyScript,
                                                          ReporterModel reporter,
                                                          Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                var conf = new CompilerConfiguration();
                var binding = new Binding();
                binding.setProperty("network", network);
                var shell = new GroovyShell(binding, conf);
                shell.evaluate(groovyScript);

                subReporter.report(Report.builder()
                    .withKey("groovyScriptApplied")
                    .withDefaultMessage("Groovy script applied")
                    .withSeverity(new TypedValue("GROOVY_SCRIPT_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the groovy script modification entity to the listener
            listener.storeGroovyScriptModification(groovyScript);
        }, GROOVY_SCRIPT_ERROR, networkUuid, reporter, subReporter);
    }

    public Flux<ModificationInfos> applyGroovyScript(UUID networkUuid, String variantId, UUID groupUuid, String groovyScript) {
        return assertGroovyScriptNotEmpty(groovyScript).thenMany(
            getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                Reporter subReporter = reporter.createSubReporter("GroovyScript", "Apply groovy script");

                return execApplyGroovyScript(listener, groovyScript, reporter, subReporter);
            })
        );
    }

    private List<EquipmenModificationInfos> execChangeSwitchState(NetworkStoreListener listener,
                                                                  String switchId,
                                                                  boolean open,
                                                                  ReporterModel reporter,
                                                                  Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Switch aSwitch = network.getSwitch(switchId);
                if (aSwitch == null) {
                    throw new NetworkModificationException(SWITCH_NOT_FOUND, switchId);
                }
                if (aSwitch.isOpen() != open) {
                    aSwitch.setOpen(open);
                }

                subReporter.report(Report.builder()
                    .withKey("switchChanged")
                    .withDefaultMessage("Switch with id=${id} open state changed")
                    .withValue("id", switchId)
                    .withSeverity(new TypedValue("SWITCH_OPEN_STATE_CHANGED_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the switch 'open' attribute modification entity to the listener
            listener.storeEquipmentAttributeModification(switchId, "open", open);
        }, MODIFICATION_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmenModificationInfos> changeSwitchState(UUID networkUuid, String variantId, UUID groupUuid, String switchId, boolean open) {
        return getNetworkModificationInfos(networkUuid, variantId)
            .flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                Reporter subReporter = reporter.createSubReporter("SwitchChange", "Switch state change");

                return execChangeSwitchState(listener, switchId, open, reporter, subReporter);
            });
    }

    public Flux<UUID> getModificationGroups() {
        return Flux.fromStream(() -> modificationRepository.getModificationGroupsUuids().stream());
    }

    public Flux<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata) {
        return Flux.fromStream(() -> modificationRepository.getModifications(groupUuid, onlyMetadata).stream());
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();
        return terminal1Disconnected && terminal2Disconnected;
    }

    public Flux<ModificationInfos> changeLineStatus(UUID networkUuid, String variantId, UUID groupUuid, String lineId, String action) {
        return assertBranchActionValid(action).thenMany(
            getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                return execChangeLineStatus(listener, lineId, BranchStatusModificationInfos.ActionType.valueOf(action.toUpperCase()), reporter);
            })
        );
    }

    private List<ModificationInfos> execChangeLineStatus(NetworkStoreListener listener, String lineId, BranchStatusModificationInfos.ActionType action, ReporterModel reporter) {
        if (listener.getNetwork().getLine(lineId) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
        }
        switch (action) {
            case LOCKOUT:
                return execLockoutLine(listener, lineId, reporter);
            case TRIP:
                return execTripLine(listener, lineId, reporter);
            case SWITCH_ON:
                return execSwitchOnLine(listener, lineId, reporter);
            case ENERGISE_END_ONE:
                return execEnergiseLineEnd(listener, lineId, Branch.Side.ONE, reporter);
            case ENERGISE_END_TWO:
                return execEnergiseLineEnd(listener, lineId, Side.TWO, reporter);
            default:
                throw NetworkModificationException.createBranchActionTypeUnsupported(action);
        }
    }

    private List<ModificationInfos> execLockoutLine(NetworkStoreListener listener, String lineId, ReporterModel reporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        Reporter subReporter = reporter.createSubReporter("LineLockout", "Lockout line");

        return doAction(listener, () -> {
                if (listener.isApplyModifications()) {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends");
                    }

                    subReporter.report(Report.builder()
                        .withKey("lockoutLineApplied")
                        .withDefaultMessage("Line ${id} (id) : lockout applied")
                        .withValue("id", lineId)
                        .withSeverity(new TypedValue(LINE_STATUS_INFO_REPORT_SEVERITY, TypedValue.INFO_LOGLEVEL))
                        .build());
                }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, BranchStatusModificationInfos.ActionType.LOCKOUT);
            }, MODIFICATION_ERROR, networkUuid, reporter, subReporter
        );
    }

    private List<ModificationInfos> execTripLine(NetworkStoreListener listener, String lineId, ReporterModel reporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        Reporter subReporter = reporter.createSubReporter("LineTrip", "Trip line");

        return doAction(listener, () -> {
                if (listener.isApplyModifications()) {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends");
                    }

                    subReporter.report(Report.builder()
                        .withKey("tripLineApplied")
                        .withDefaultMessage("Line ${id} (id) : trip applied")
                        .withValue("id", lineId)
                        .withSeverity(new TypedValue(LINE_STATUS_INFO_REPORT_SEVERITY, TypedValue.INFO_LOGLEVEL))
                        .build());
                }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, BranchStatusModificationInfos.ActionType.TRIP);
            }, MODIFICATION_ERROR, networkUuid, reporter, subReporter
        );
    }

    private List<ModificationInfos> execEnergiseLineEnd(NetworkStoreListener listener,
                                                                    String lineId,
                                                                    Branch.Side side,
                                                                    ReporterModel reporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        Reporter subReporter = reporter.createSubReporter("LineEnergise", "Energise line");

        return doAction(listener, () -> {
                if (listener.isApplyModifications()) {
                    Terminal terminalToConnect = network.getLine(lineId).getTerminal(side);
                    boolean isTerminalToConnectConnected = terminalToConnect.isConnected() || terminalToConnect.connect();
                    Terminal terminalToDisconnect = network.getLine(lineId).getTerminal(side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE);
                    boolean isTerminalToDisconnectDisconnected = !terminalToDisconnect.isConnected() || terminalToDisconnect.disconnect();
                    if (isTerminalToConnectConnected && isTerminalToDisconnectDisconnected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise line end");
                    }

                    subReporter.report(Report.builder()
                        .withKey("energiseLineEndApplied")
                        .withDefaultMessage("Line ${id} (id) : energise the side ${side} applied")
                        .withValue("id", lineId)
                        .withValue("side", side.name())
                        .withSeverity(new TypedValue(LINE_STATUS_INFO_REPORT_SEVERITY, TypedValue.INFO_LOGLEVEL))
                        .build());
                }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, side == Branch.Side.ONE ? BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE : BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO);
            }, MODIFICATION_ERROR, networkUuid, reporter, subReporter
        );
    }

    private List<ModificationInfos> execSwitchOnLine(NetworkStoreListener listener, String lineId, ReporterModel reporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        Reporter subReporter = reporter.createSubReporter("LineSwitchOn", "Switch on line");

        return doAction(listener, () -> {
                if (listener.isApplyModifications()) {
                    Terminal terminal1 = network.getLine(lineId).getTerminal1();
                    boolean terminal1Connected = terminal1.isConnected() || terminal1.connect();
                    Terminal terminal2 = network.getLine(lineId).getTerminal2();
                    boolean terminal2Connected = terminal2.isConnected() || terminal2.connect();
                    if (terminal1Connected && terminal2Connected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to connect both line ends");
                    }

                    subReporter.report(Report.builder()
                        .withKey("switchOnLineApplied")
                        .withDefaultMessage("Line ${id} (id) : switch on applied")
                        .withValue("id", lineId)
                        .withSeverity(new TypedValue(LINE_STATUS_INFO_REPORT_SEVERITY, TypedValue.INFO_LOGLEVEL))
                        .build());
                }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, BranchStatusModificationInfos.ActionType.SWITCH_ON);
            }, MODIFICATION_ERROR, networkUuid, reporter, subReporter
        );
    }

    public Mono<Void> deleteModificationGroup(UUID groupUuid) {
        return Mono.fromRunnable(() -> modificationRepository.deleteModificationGroup(groupUuid));
    }

    private List<ModificationInfos> doAction(NetworkStoreListener listener, Runnable action,
                                             NetworkModificationException.Type typeIfError,
                                             UUID networkUuid, ReporterModel reporter,
                                             Reporter subReporter) {
        try {
            action.run();
            if (!listener.isBuild()) {
                saveModifications(listener);
            }
            return listener.isApplyModifications() ? listener.getModifications() : Collections.emptyList();
        } catch (Exception e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(typeIfError, e);
            subReporter.report(Report.builder()
                .withKey(typeIfError.name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(new TypedValue("NETWORK_MODIFICATION_ERROR", TypedValue.ERROR_LOGLEVEL))
                .build());
            if (!listener.isBuild()) {
                throw exc;
            } else {
                return Collections.emptyList();
            }
        } finally {
            if (!listener.isBuild()) {
                // send report
                sendReport(networkUuid, reporter);
            }
        }
    }

    private void saveModifications(NetworkStoreListener listener) {
        listener.saveModifications();
        try {
            networkStoreService.flush(listener.getNetwork());
        } catch (Exception e) {
            listener.deleteModifications();
            throw e;
        }
    }

    private Mono<ModificationNetworkInfos> getNetworkModificationInfos(UUID networkUuid, String variantId) {
        return Mono.fromCallable(() -> {
            Network network;
            try {
                network = networkStoreService.getNetwork(networkUuid);
            } catch (PowsyblException e) {
                throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
            }
            boolean applyModifications = true;
            if (variantId != null) {
                if (network.getVariantManager().getVariantIds().stream().anyMatch(id -> id.equals(variantId))) {
                    network.getVariantManager().setWorkingVariant(variantId);
                } else {
                    applyModifications = false;
                }
            }
            return new ModificationNetworkInfos(network, applyModifications);
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private Mono<Void> assertGroovyScriptNotEmpty(String groovyScript) {
        return StringUtils.isBlank(groovyScript) ? Mono.error(new NetworkModificationException(GROOVY_SCRIPT_EMPTY)) : Mono.empty();
    }

    private Mono<Void> assertBranchActionValid(String action) {
        if (StringUtils.isBlank(action)) {
            return Mono.error(new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY));
        }
        try {
            BranchStatusModificationInfos.ActionType.valueOf(action.toUpperCase());
        } catch (IllegalArgumentException e) {
            return Mono.error(NetworkModificationException.createBranchActionTypeUnknown(action));
        }
        return Mono.empty();
    }

    private VoltageLevel getVoltageLevel(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId);
        }
        return voltageLevel;
    }

    private Bus getBusBreakerBus(VoltageLevel voltageLevel, String busId) {
        VoltageLevel.BusBreakerView busBreakerView = voltageLevel.getBusBreakerView();
        Bus bus = busBreakerView.getBus(busId);
        if (bus == null) {
            throw new NetworkModificationException(BUS_NOT_FOUND, busId);
        }
        return bus;
    }

    private int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
                                               String equipmentName) {
        return createNodeBreakerCellSwitches(voltageLevel, busBarSectionId, equipmentId, equipmentName, "");
    }

    private int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
                                               String equipmentName, String sideSuffix) {
        VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
        BusbarSection busbarSection = nodeBreakerView.getBusbarSection(busBarSectionId);
        if (busbarSection == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busBarSectionId);
        }

        // creating the disconnector
        int newNode = nodeBreakerView.getMaximumNodeIndex();
        String disconnectorId = "disconnector_" + equipmentId + sideSuffix;
        String disconnectorName = equipmentName != null ? "disconnector_" + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
                .setId(disconnectorId)
                .setName(disconnectorName)
                .setKind(SwitchKind.DISCONNECTOR)
                .setRetained(false)
                .setOpen(false)
                .setFictitious(false)
                .setNode1(busbarSection.getTerminal().getNodeBreakerView().getNode())
                .setNode2(newNode + 1)
                .add();

        // creating the breaker
        String breakerId = "breaker_" + equipmentId + sideSuffix;
        String breakerName = equipmentName != null ? "breaker_" + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(breakerId)
            .setName(breakerName)
            .setKind(SwitchKind.BREAKER)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(newNode + 1)
            .setNode2(newNode + 2)
            .add();

        return newNode + 2;
    }

    private Load createLoadInNodeBreaker(VoltageLevel voltageLevel, LoadCreationInfos loadCreationInfos) {
        // create cell switches
        int nodeNum = createNodeBreakerCellSwitches(voltageLevel, loadCreationInfos.getBusOrBusbarSectionId(),
            loadCreationInfos.getEquipmentId(),
            loadCreationInfos.getEquipmentName());

        // creating the load
        return voltageLevel.newLoad()
            .setId(loadCreationInfos.getEquipmentId())
            .setName(loadCreationInfos.getEquipmentName())
            .setLoadType(loadCreationInfos.getLoadType())
            .setNode(nodeNum)
            .setP0(loadCreationInfos.getActivePower())
            .setQ0(loadCreationInfos.getReactivePower())
            .add();
    }

    private Load createLoadInBusBreaker(VoltageLevel voltageLevel, LoadCreationInfos loadCreationInfos) {
        Bus bus = getBusBreakerBus(voltageLevel, loadCreationInfos.getBusOrBusbarSectionId());

        // creating the load
        return voltageLevel.newLoad()
            .setId(loadCreationInfos.getEquipmentId())
            .setName(loadCreationInfos.getEquipmentName())
            .setLoadType(loadCreationInfos.getLoadType())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(loadCreationInfos.getActivePower())
            .setQ0(loadCreationInfos.getReactivePower())
            .add();
    }

    private List<EquipmenModificationInfos> execCreateLoad(NetworkStoreListener listener,
                                                           LoadCreationInfos loadCreationInfos,
                                                           ReporterModel reporter,
                                                           Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the load in the network
                VoltageLevel voltageLevel = getVoltageLevel(network, loadCreationInfos.getVoltageLevelId());
                if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    createLoadInNodeBreaker(voltageLevel, loadCreationInfos);
                } else {
                    createLoadInBusBreaker(voltageLevel, loadCreationInfos);
                }

                subReporter.report(Report.builder()
                    .withKey("loadCreated")
                    .withDefaultMessage("New load with id=${id} created")
                    .withValue("id", loadCreationInfos.getEquipmentId())
                    .withSeverity(new TypedValue("LOAD_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the load creation entity to the listener
            listener.storeLoadCreation(loadCreationInfos);
        }, CREATE_LOAD_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmenModificationInfos> createLoad(UUID networkUuid, String variantId, UUID groupUuid, LoadCreationInfos loadCreationInfos) {
        return assertLoadCreationInfosNotEmpty(loadCreationInfos).thenMany(
            getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                Reporter subReporter = reporter.createSubReporter("LoadCreation", "Load creation");

                return execCreateLoad(listener, loadCreationInfos, reporter, subReporter);
            }));
    }

    private Mono<Void> assertLoadCreationInfosNotEmpty(LoadCreationInfos loadCreationInfos) {
        return loadCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_LOAD_ERROR, "Missing required attributes to create the load")) : Mono.empty();
    }

    private List<EquipmentDeletionInfos> execDeleteEquipment(NetworkStoreListener listener,
                                                             String equipmentType,
                                                             String equipmentId,
                                                             ReporterModel reporter,
                                                             Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Identifiable<?> identifiable = null;
                switch (EquipmentType.valueOf(equipmentType)) {
                    case HVDC_LINE:
                        identifiable = network.getHvdcLine(equipmentId);
                        break;
                    case LINE:
                        identifiable = network.getLine(equipmentId);
                        break;
                    case TWO_WINDINGS_TRANSFORMER:
                        identifiable = network.getTwoWindingsTransformer(equipmentId);
                        break;
                    case THREE_WINDINGS_TRANSFORMER:
                        identifiable = network.getThreeWindingsTransformer(equipmentId);
                        break;
                    case GENERATOR:
                        identifiable = network.getGenerator(equipmentId);
                        break;
                    case LOAD:
                        identifiable = network.getLoad(equipmentId);
                        break;
                    case BATTERY:
                        identifiable = network.getBattery(equipmentId);
                        break;
                    case SHUNT_COMPENSATOR:
                        identifiable = network.getShuntCompensator(equipmentId);
                        break;
                    case STATIC_VAR_COMPENSATOR:
                        identifiable = network.getStaticVarCompensator(equipmentId);
                        break;
                    case DANGLING_LINE:
                        identifiable = network.getDanglingLine(equipmentId);
                        break;
                    case HVDC_CONVERTER_STATION:
                        identifiable = network.getHvdcConverterStation(equipmentId);
                        break;
                    case SUBSTATION:
                        identifiable = network.getSubstation(equipmentId);
                        break;
                    case VOLTAGE_LEVEL:
                        identifiable = network.getVoltageLevel(equipmentId);
                        break;
                    default:
                        break;
                }
                if (identifiable == null) {
                    throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + equipmentId + " not found or of bad type");
                }

                // store the substations ids in the listener

                Set<String> substationIds = Set.of();
                // On substation deletion, the substation id isn't set in the substations to be updated.
                // If later we handle automatic lines deletion (i.e. on substation deletion, we remove all the lines connected to the substation), we'll have to set
                // the adjacent substations in the substations to be updated.
                if (!(identifiable instanceof Substation)) {
                    substationIds = NetworkStoreListener.getSubstationIds(identifiable);
                }

                if (identifiable instanceof Connectable) {
                    ((Connectable) identifiable).remove();
                } else if (identifiable instanceof HvdcLine) {
                    ((HvdcLine) identifiable).remove();
                } else if (identifiable instanceof VoltageLevel) {
                    ((VoltageLevel) identifiable).remove();
                } else if (identifiable instanceof Substation) {
                    ((Substation) identifiable).remove();
                }

                // Done here, and not in the network listener onRemoval method
                // because onRemoval must be refactored in powsybl core
                listener.onTemporaryRemoval(equipmentId, substationIds);

                subReporter.report(Report.builder()
                    .withKey("equipmentDeleted")
                    .withDefaultMessage("equipment of type=${type} and id=${id} deleted")
                    .withValue("type", equipmentType)
                    .withValue("id", equipmentId)
                    .withSeverity(new TypedValue("EQUIPMENT_DELETION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the equipment deletion entity to the listener
            listener.storeEquipmentDeletion(equipmentId, equipmentType);
        }, DELETE_EQUIPMENT_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmentDeletionInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmentDeletionInfos> deleteEquipment(UUID networkUuid, String variantId, UUID groupUuid, String equipmentType, String equipmentId) {
        return getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
            NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
            ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
            Reporter subReporter = reporter.createSubReporter("EquipmentDeletion", "Equipment deletion");

            return execDeleteEquipment(listener, equipmentType, equipmentId, reporter, subReporter);
        });
    }

    private void sendReport(UUID networkUuid, ReporterModel reporter) {
        var headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        var resourceUrl = DELIMITER + REPORT_API_VERSION + DELIMITER + "reports" + DELIMITER + networkUuid.toString();
        var uriBuilder = UriComponentsBuilder.fromPath(resourceUrl);
        try {
            reportServerRest.exchange(uriBuilder.toUriString(), HttpMethod.PUT, new HttpEntity<>(objectMapper.writeValueAsString(reporter), headers), ReporterModel.class);
        } catch (JsonProcessingException error) {
            throw new PowsyblException("error creating report", error);
        }
    }

    public void setReportServerRest(RestTemplate reportServerRest) {
        this.reportServerRest = Objects.requireNonNull(reportServerRest, "reportServerRest can't be null");
    }

    private Generator createGeneratorInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos) {
        // create cell switches
        int nodeNum = createNodeBreakerCellSwitches(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId(),
            generatorCreationInfos.getEquipmentId(),
            generatorCreationInfos.getEquipmentName());

        // creating the generator
        return voltageLevel.newGenerator()
            .setId(generatorCreationInfos.getEquipmentId())
            .setName(generatorCreationInfos.getEquipmentName())
            .setEnergySource(generatorCreationInfos.getEnergySource())
            .setNode(nodeNum)
            .setMinP(generatorCreationInfos.getMinActivePower())
            .setMaxP(generatorCreationInfos.getMaxActivePower())
            .setRatedS(generatorCreationInfos.getRatedNominalPower() != null ? generatorCreationInfos.getRatedNominalPower() : Double.NaN)
            .setTargetP(generatorCreationInfos.getActivePowerSetpoint())
            .setTargetQ(generatorCreationInfos.getReactivePowerSetpoint() != null ? generatorCreationInfos.getReactivePowerSetpoint() : Double.NaN)
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(generatorCreationInfos.getVoltageSetpoint() != null ? generatorCreationInfos.getVoltageSetpoint() : Double.NaN)
            .add();
    }

    private Generator createGeneratorInBusBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos) {
        Bus bus = getBusBreakerBus(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId());

        // creating the generator
        return voltageLevel.newGenerator()
            .setId(generatorCreationInfos.getEquipmentId())
            .setName(generatorCreationInfos.getEquipmentName())
            .setEnergySource(generatorCreationInfos.getEnergySource())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setMinP(generatorCreationInfos.getMinActivePower())
            .setMaxP(generatorCreationInfos.getMaxActivePower())
            .setRatedS(generatorCreationInfos.getRatedNominalPower() != null ? generatorCreationInfos.getRatedNominalPower() : Double.NaN)
            .setTargetP(generatorCreationInfos.getActivePowerSetpoint())
            .setTargetQ(generatorCreationInfos.getReactivePowerSetpoint() != null ? generatorCreationInfos.getReactivePowerSetpoint() : Double.NaN)
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(generatorCreationInfos.getVoltageSetpoint() != null ? generatorCreationInfos.getVoltageSetpoint() : Double.NaN)
            .add();
    }

    private List<EquipmenModificationInfos> execCreateGenerator(NetworkStoreListener listener,
                                                                GeneratorCreationInfos generatorCreationInfos,
                                                                ReporterModel reporter,
                                                                Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the generator in the network
                VoltageLevel voltageLevel = getVoltageLevel(network, generatorCreationInfos.getVoltageLevelId());
                if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    createGeneratorInNodeBreaker(voltageLevel, generatorCreationInfos);
                } else {
                    createGeneratorInBusBreaker(voltageLevel, generatorCreationInfos);
                }

                subReporter.report(Report.builder()
                    .withKey("generatorCreated")
                    .withDefaultMessage("New generator with id=${id} created")
                    .withValue("id", generatorCreationInfos.getEquipmentId())
                    .withSeverity(new TypedValue("GENERATOR_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the generator creation entity to the listener
            listener.storeGeneratorCreation(generatorCreationInfos);
        }, CREATE_GENERATOR_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmenModificationInfos> createGenerator(UUID networkUuid, String variantId, UUID groupUuid, GeneratorCreationInfos generatorCreationInfos) {
        return assertGeneratorCreationInfosNotEmpty(generatorCreationInfos).thenMany(
            getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                Reporter subReporter = reporter.createSubReporter("GeneratorCreation", "Generator creation");

                return execCreateGenerator(listener, generatorCreationInfos, reporter, subReporter);
            }));
    }

    private Mono<Void> assertGeneratorCreationInfosNotEmpty(GeneratorCreationInfos generatorCreationInfos) {
        return generatorCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Missing required attributes to create the generator")) : Mono.empty();
    }

    private void setBranchAdderNodeOrBus(BranchAdder<?> branchAdder, VoltageLevel voltageLevel, BranchCreationInfos branchCreationInfos, Side side) {
        String currentBusBarSectionId = (side == Side.ONE) ? branchCreationInfos.getBusOrBusbarSectionId1() : branchCreationInfos.getBusOrBusbarSectionId2();

        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
            // busId is a busbar section id
            BusbarSection busbarSection = nodeBreakerView.getBusbarSection(currentBusBarSectionId);
            if (busbarSection == null) {
                throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, currentBusBarSectionId);
            }

            // create cell switches
            String sideSuffix = side != null ? "_" + side.name() : "";
            int nodeNum = createNodeBreakerCellSwitches(voltageLevel,
                currentBusBarSectionId,
                    branchCreationInfos.getEquipmentId(),
                    branchCreationInfos.getEquipmentName(),
                sideSuffix);

            // complete the lineAdder
            if (side == Side.ONE) {
                branchAdder.setNode1(nodeNum);
            } else {
                branchAdder.setNode2(nodeNum);
            }
        } else { // BUS BREAKER
            // busId is a bus id
            Bus bus = getBusBreakerBus(voltageLevel, currentBusBarSectionId);

            // complete the lineAdder
            if (side == Side.ONE) {
                branchAdder.setBus1(bus.getId()).setConnectableBus1(bus.getId());
            } else {
                branchAdder.setBus2(bus.getId()).setConnectableBus2(bus.getId());
            }
        }

    }

    private Line createLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos) {

        // common settings
        LineAdder lineAdder = network.newLine()
                                .setId(lineCreationInfos.getEquipmentId())
                                .setName(lineCreationInfos.getEquipmentName())
                                .setVoltageLevel1(lineCreationInfos.getVoltageLevelId1())
                                .setVoltageLevel2(lineCreationInfos.getVoltageLevelId2())
                                .setR(lineCreationInfos.getSeriesResistance())
                                .setX(lineCreationInfos.getSeriesReactance())
                                .setG1(lineCreationInfos.getShuntConductance1() != null ? lineCreationInfos.getShuntConductance1() : 0.0)
                                .setB1(lineCreationInfos.getShuntSusceptance1() != null ? lineCreationInfos.getShuntSusceptance1() : 0.0)
                                .setG2(lineCreationInfos.getShuntConductance2() != null ? lineCreationInfos.getShuntConductance2() : 0.0)
                                .setB2(lineCreationInfos.getShuntSusceptance2() != null ? lineCreationInfos.getShuntSusceptance2() : 0.0);

        // lineAdder completion by topology
        setBranchAdderNodeOrBus(lineAdder, voltageLevel1, lineCreationInfos, Side.ONE);
        setBranchAdderNodeOrBus(lineAdder, voltageLevel2, lineCreationInfos, Side.TWO);

        return lineAdder.add();
    }

    private List<EquipmenModificationInfos> execCreateLine(NetworkStoreListener listener,
                                                           LineCreationInfos lineCreationInfos,
                                                           ReporterModel reporter,
                                                           Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the line in the network
                VoltageLevel voltageLevel1 = getVoltageLevel(network, lineCreationInfos.getVoltageLevelId1());
                VoltageLevel voltageLevel2 = getVoltageLevel(network, lineCreationInfos.getVoltageLevelId2());

                Line myLine = createLine(network, voltageLevel1, voltageLevel2, lineCreationInfos);

                // Set Permanent Current Limits if exist
                CurrentLimitsInfos currentLimitsInfos1 = lineCreationInfos.getCurrentLimits1();
                CurrentLimitsInfos currentLimitsInfos2 = lineCreationInfos.getCurrentLimits2();

                if (currentLimitsInfos1 != null && currentLimitsInfos1.getPermanentLimit() != null) {
                    myLine.newCurrentLimits1().setPermanentLimit(currentLimitsInfos1.getPermanentLimit()).add();
                }
                if (currentLimitsInfos2 != null && currentLimitsInfos2.getPermanentLimit() != null) {
                    myLine.newCurrentLimits2().setPermanentLimit(currentLimitsInfos2.getPermanentLimit()).add();
                }

                subReporter.report(Report.builder()
                    .withKey("lineCreated")
                    .withDefaultMessage("New line with id=${id} created")
                    .withValue("id", lineCreationInfos.getEquipmentId())
                    .withSeverity(new TypedValue("LINE_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the line creation entity to the listener
            listener.storeLineCreation(lineCreationInfos);
        }, CREATE_LINE_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmenModificationInfos> createLine(UUID networkUuid, String variantId, UUID groupUuid, LineCreationInfos lineCreationInfos) {
        return assertLineCreationInfosNotEmpty(lineCreationInfos).thenMany(
            getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                Reporter subReporter = reporter.createSubReporter("LineCreation", "Line creation");

                return execCreateLine(listener, lineCreationInfos, reporter, subReporter);
            }));
    }

    private Mono<Void> assertLineCreationInfosNotEmpty(LineCreationInfos lineCreationInfos) {
        return lineCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_LINE_ERROR, "Missing required attributes to create the line")) : Mono.empty();
    }

    private List<EquipmenModificationInfos> execCreateTwoWindingsTransformer(NetworkStoreListener listener,
                                                                             TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos,
                                                                             ReporterModel reporter,
                                                                             Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the 2wt in the network
                VoltageLevel voltageLevel1 = getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId1());
                VoltageLevel voltageLevel2 = getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId2());

                createTwoWindingsTransformer(network, voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos);

                subReporter.report(Report.builder()
                    .withKey("twoWindingsTransformerCreated")
                    .withDefaultMessage("New two windings transformer with id=${id} created")
                    .withValue("id", twoWindingsTransformerCreationInfos.getEquipmentId())
                    .withSeverity(new TypedValue("TWO_WINDINGS_TRANSFORMER_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the 2wt creation entity to the listener
            listener.storeTwoWindingsTransformerCreation(twoWindingsTransformerCreationInfos);
        }, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmenModificationInfos> createTwoWindingsTransformer(UUID networkUuid, String variantId, UUID groupUuid, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        return assertTwoWindingsTransformerCreationInfosNotEmpty(twoWindingsTransformerCreationInfos).thenMany(
            getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                Reporter subReporter = reporter.createSubReporter("TwoWindingsTransformerCreation", "Two windings transformer creation");

                return execCreateTwoWindingsTransformer(listener, twoWindingsTransformerCreationInfos, reporter, subReporter);
            }));
    }

    private TwoWindingsTransformer createTwoWindingsTransformer(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        TwoWindingsTransformerAdder twoWindingsTransformerAdder;
        Optional<Substation> optS1 = voltageLevel1.getSubstation();
        Optional<Substation> optS2 = voltageLevel2.getSubstation();
        Substation s1 = optS1.orElse(null);
        Substation s2 = optS2.orElse(null);
        BranchAdder<TwoWindingsTransformerAdder> branchAdder;

        if (s1 != null) {
            branchAdder = s1.newTwoWindingsTransformer();
        } else if (s2 != null) {
            branchAdder = s2.newTwoWindingsTransformer();
        } else {
            branchAdder = network.newTwoWindingsTransformer();
        }
        // common settings
        twoWindingsTransformerAdder = branchAdder.setId(twoWindingsTransformerCreationInfos.getEquipmentId())
                .setName(twoWindingsTransformerCreationInfos.getEquipmentName())
                .setVoltageLevel1(twoWindingsTransformerCreationInfos.getVoltageLevelId1())
                .setVoltageLevel2(twoWindingsTransformerCreationInfos.getVoltageLevelId2())
                .setG(twoWindingsTransformerCreationInfos.getMagnetizingConductance())
                .setB(twoWindingsTransformerCreationInfos.getMagnetizingSusceptance())
                .setR(twoWindingsTransformerCreationInfos.getSeriesResistance())
                .setX(twoWindingsTransformerCreationInfos.getSeriesReactance())
                .setRatedU1(twoWindingsTransformerCreationInfos.getRatedVoltage1())
                .setRatedU2(twoWindingsTransformerCreationInfos.getRatedVoltage2());

        // BranchAdder completion by topology
        setBranchAdderNodeOrBus(branchAdder, voltageLevel1, twoWindingsTransformerCreationInfos, Side.ONE);
        setBranchAdderNodeOrBus(branchAdder, voltageLevel2, twoWindingsTransformerCreationInfos, Side.TWO);

        return twoWindingsTransformerAdder.add();
    }

    private Mono<Void> assertTwoWindingsTransformerCreationInfosNotEmpty(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        return twoWindingsTransformerCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Missing required attributes to create the two windings transformer")) : Mono.empty();
    }

    private List<EquipmenModificationInfos> execCreateSubstation(NetworkStoreListener listener,
                                                                 SubstationCreationInfos substationCreationInfos,
                                                                 ReporterModel reporter,
                                                                 Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                network.newSubstation()
                    .setId(substationCreationInfos.getEquipmentId())
                    .setName(substationCreationInfos.getEquipmentName())
                    .setCountry(substationCreationInfos.getSubstationCountry())
                    .add();

                subReporter.report(Report.builder()
                    .withKey("substationCreated")
                    .withDefaultMessage("New substation with id=${id} created")
                    .withValue("id", substationCreationInfos.getEquipmentId())
                    .withSeverity(new TypedValue("SUBSTATION_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());
            }

            // add the substation creation entity to the listener
            listener.storeSubstationCreation(substationCreationInfos);
        }, CREATE_SUBSTATION_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public Flux<EquipmenModificationInfos> createSubstation(UUID networkUuid, String variantId, UUID groupUuid, SubstationCreationInfos substationCreationInfos) {
        return assertSubstationCreationInfosNotEmpty(substationCreationInfos).thenMany(
                getNetworkModificationInfos(networkUuid, variantId).flatMapIterable(networkInfos -> {
                    NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, modificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
                    ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
                    Reporter subReporter = reporter.createSubReporter("SubstationCreation", "Substation creation");

                    return execCreateSubstation(listener, substationCreationInfos, reporter, subReporter);
                }));
    }

    private Mono<Void> assertSubstationCreationInfosNotEmpty(SubstationCreationInfos substationCreationInfos) {
        return substationCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Missing required attributes to create the substation")) : Mono.empty();
    }

    public Mono<Network> cloneNetworkVariant(UUID networkUuid, String originVariantId, String destinationVariantId) {
        return Mono.fromCallable(() -> {
            Network network;
            try {
                network = networkStoreService.getNetwork(networkUuid);
            } catch (PowsyblException e) {
                throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
            }
            String startingVariant = StringUtils.isBlank(originVariantId) ? VariantManagerConstants.INITIAL_VARIANT_ID : originVariantId;
            try {
                network.getVariantManager().cloneVariant(startingVariant, destinationVariantId, true);  // cloning variant
                network.getVariantManager().setWorkingVariant(destinationVariantId);  // set current variant to destination variant
            } catch (PowsyblException e) {
                throw new NetworkModificationException(VARIANT_NOT_FOUND, startingVariant);
            }
            return network;
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private void changeSwitchAttribute(Switch aSwitch, String attributeName, Object attributeValue) {
        if (attributeName.equals("open") && Boolean.TRUE.equals(aSwitch.isOpen() != (Boolean) attributeValue)) {
            aSwitch.setOpen((Boolean) attributeValue);
        }
    }

    private void changeGeneratorAttribute(Generator generator, String attributeName, Object attributeValue) {
        if (attributeName.equals("targetP")) {
            generator.setTargetP((Double) attributeValue);
        }
    }

    private void changeLineAttribute(Line line, String attributeName, Object attributeValue) {
        if (attributeName.equals("branchStatus")) {
            line.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.valueOf((String) attributeValue)).add();
        }
    }

    private void changeTwoWindingsTransformerAttribute(TwoWindingsTransformer transformer, String attributeName, Object attributeValue) {
        if (attributeName.equals("ratioTapChanger.tapPosition")) {
            transformer.getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
        } else if (attributeName.equals("phaseTapChanger.tapPosition")) {
            transformer.getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
        }
    }

    private void changeThreeWindingsTransformerAttribute(ThreeWindingsTransformer transformer, String attributeName, Object attributeValue) {
        if (attributeName.equals("ratioTapChanger1.tapPosition")) {
            transformer.getLeg1().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
        } else if (attributeName.equals("ratioTapChanger2.tapPosition")) {
            transformer.getLeg2().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
        } else if (attributeName.equals("ratioTapChanger3.tapPosition")) {
            transformer.getLeg3().getOptionalRatioTapChanger().ifPresent(r -> r.setTapPosition((Integer) attributeValue));
        } else if (attributeName.equals("phaseTapChanger1.tapPosition")) {
            transformer.getLeg1().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
        } else if (attributeName.equals("phaseTapChanger2.tapPosition")) {
            transformer.getLeg2().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
        } else if (attributeName.equals("phaseTapChanger3.tapPosition")) {
            transformer.getLeg3().getOptionalPhaseTapChanger().ifPresent(p -> p.setTapPosition((Integer) attributeValue));
        }
    }

    private List<EquipmenModificationInfos> execChangeEquipmentAttribute(NetworkStoreListener listener,
                                                                         String equipmentId,
                                                                         String attributeName,
                                                                         Object attributeValue,
                                                                         ReporterModel reporter,
                                                                         Reporter subReporter) {
        Network network = listener.getNetwork();
        UUID networkUuid = listener.getNetworkUuid();

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Identifiable<?> identifiable = network.getIdentifiable(equipmentId);
                if (identifiable == null) {
                    throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, equipmentId);
                }
                if (identifiable instanceof Switch) {
                    changeSwitchAttribute((Switch) identifiable, attributeName, attributeValue);
                } else if (identifiable instanceof Injection) {
                    if (identifiable instanceof Generator) {
                        changeGeneratorAttribute((Generator) identifiable, attributeName, attributeValue);
                    }
                } else if (identifiable instanceof Branch) {
                    if (identifiable instanceof Line) {
                        changeLineAttribute((Line) identifiable, attributeName, attributeValue);
                    } else if (identifiable instanceof TwoWindingsTransformer) {
                        changeTwoWindingsTransformerAttribute((TwoWindingsTransformer) identifiable, attributeName, attributeValue);
                    }
                } else if (identifiable instanceof ThreeWindingsTransformer) {
                    changeThreeWindingsTransformerAttribute((ThreeWindingsTransformer) identifiable, attributeName, attributeValue);
                } else if (identifiable instanceof HvdcLine) {
                    // no hvdc line modifications yet
                }
            }
        }, MODIFICATION_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmenModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<ModificationInfos> applyModifications(Network network, UUID networkUuid, BuildInfos buildInfos) {
        // Apply all modifications belonging to the modification groups uuids in buildInfos
        List<ModificationInfos> allModificationsInfos = new ArrayList<>();
        NetworkStoreListener listener = NetworkStoreListener.create(network,
            networkUuid,
            null,
            modificationRepository,
            equipmentInfosService,
            true,
            true);
        ReporterModel reporter = new ReporterModel(NETWORK_BUILD_REPORT_KEY, NETWORK_BUILD_REPORT_NAME);

        modificationRepository.getModificationsEntities(buildInfos.getModifications()).forEach(modificationEntity -> {
            if (!modificationEntity.isActive()) {
                return;
            }
            ModificationType type = ModificationType.valueOf(modificationEntity.getType());
            switch (type) {
                case EQUIPMENT_ATTRIBUTE_MODIFICATION: {
                    EquipmentAttributeModificationEntity<?> attributeModificationEntity = (EquipmentAttributeModificationEntity<?>) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("AttributeModification", "Attribute modification");
                    List<EquipmenModificationInfos> modificationInfos = execChangeEquipmentAttribute(listener, attributeModificationEntity.getEquipmentId(), attributeModificationEntity.getAttributeName(), attributeModificationEntity.getAttributeValue(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case LOAD_CREATION: {
                    LoadCreationEntity loadCreationEntity = (LoadCreationEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("LoadCreation", "Load creation");
                    List<EquipmenModificationInfos> modificationInfos = execCreateLoad(listener, loadCreationEntity.toModificationInfos(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case GENERATOR_CREATION: {
                    GeneratorCreationEntity generatorCreationEntity = (GeneratorCreationEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("GeneratorCreation", "Generator creation");
                    List<EquipmenModificationInfos> modificationInfos = execCreateGenerator(listener, generatorCreationEntity.toModificationInfos(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case LINE_CREATION: {
                    LineCreationEntity lineCreationEntity = (LineCreationEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("LineCreation", "Line creation");
                    List<EquipmenModificationInfos> modificationInfos = execCreateLine(listener, lineCreationEntity.toModificationInfos(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case TWO_WINDINGS_TRANSFORMER_CREATION: {
                    TwoWindingsTransformerCreationEntity twoWindingsTransformerCreationEntity = (TwoWindingsTransformerCreationEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("TwoWindingsTransformerCreation", "Two windings transformer creation");
                    List<EquipmenModificationInfos> modificationInfos = execCreateTwoWindingsTransformer(listener, twoWindingsTransformerCreationEntity.toModificationInfos(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case EQUIPMENT_DELETION: {
                    EquipmentDeletionEntity deletionEntity = (EquipmentDeletionEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("EquipmentDeletion", "Equipment deletion");
                    List<EquipmentDeletionInfos> deletionInfos = execDeleteEquipment(listener, deletionEntity.getEquipmentType(), deletionEntity.getEquipmentId(), reporter, subReporter);
                    allModificationsInfos.addAll(deletionInfos);
                }
                break;

                case GROOVY_SCRIPT: {
                    GroovyScriptModificationEntity groovyModificationEntity = (GroovyScriptModificationEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("GroovyScriptModification", "Groovy script modification");
                    List<ModificationInfos> modificationInfos = execApplyGroovyScript(listener, groovyModificationEntity.getScript(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case SUBSTATION_CREATION: {
                    SubstationCreationEntity substationCreationEntity = (SubstationCreationEntity) modificationEntity;
                    Reporter subReporter = reporter.createSubReporter("SubstationCreation", "Substation creation");
                    List<EquipmenModificationInfos> modificationInfos = execCreateSubstation(listener, substationCreationEntity.toSubstationCreationInfos(), reporter, subReporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case BRANCH_STATUS: {
                    BranchStatusModificationEntity branchStatusModificationEntity = (BranchStatusModificationEntity) modificationEntity;
                    List<ModificationInfos> modificationInfos = execChangeLineStatus(listener,  branchStatusModificationEntity.getEquipmentId(),  branchStatusModificationEntity.getAction(),  reporter);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                default:
            }
        });

        // flushing network (only once at the end)
        networkStoreService.flush(listener.getNetwork());

        // send report (only once at the end)
        sendReport(networkUuid, reporter);

        return allModificationsInfos;
    }

    /*
    ** Build variant : sending message to rabbitmq
     */
    public Mono<Void> buildVariant(UUID networkUuid, BuildInfos buildInfos, String receiver) {
        return Mono.fromRunnable(() ->
            sendRunBuildMessage(new BuildExecContext(networkUuid, buildInfos, receiver).toMessage(objectMapper)));
    }

    private void sendRunBuildMessage(Message<String> message) {
        RUN_MESSAGE_LOGGER.debug("Sending message : {}", message);
        publisher.send("publishBuild-out-0", message);
    }

    public Mono<Void> stopBuild(String receiver) {
        return Mono.fromRunnable(() ->
            sendCancelBuildMessage(new BuildCancelContext(receiver).toMessage())).then();
    }

    private void sendCancelBuildMessage(Message<String> message) {
        CANCEL_MESSAGE_LOGGER.debug("Sending message : {}", message);
        publisher.send("publishCancelBuild-out-0", message);
    }

    public Mono<Void> changeModificationActiveState(@NonNull UUID groupUuid, @NonNull UUID modificationUuid, boolean active) {
        return Mono.fromRunnable(() ->
            modificationRepository.changeModificationActiveState(groupUuid, modificationUuid, active)
        );
    }
}
