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
import com.google.common.collect.Streams;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.*;
import com.powsybl.iidm.modification.topology.AttachNewLineOnLine;
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.Branch.Side;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.iidm.network.extensions.GeneratorStartupAdder;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.iidm.impl.extensions.GeneratorStartupAdderImpl;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import com.powsybl.sld.iidm.extensions.BranchStatusAdder;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
import org.gridsuite.modification.server.modifications.ModificationApplicator;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity.toEmbeddablePoints;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationService {
    private final NetworkStoreService networkStoreService;

    private final NetworkModificationRepository networkModificationRepository;

    private final ModificationApplicator modificationApplicator;

    // TO DO : transfer the use of repositories in NetworkModificationRepository
    private final ModificationGroupRepository modificationGroupRepository;
    private final ModificationRepository modificationRepository;

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
    private static final String NETWORK_MODIFICATION_REPORT_NAME = "NetworkModification";

    @Autowired
    private StreamBridge publisher;

    @Autowired
    NetworkModificationService self;

    public NetworkModificationService(@Value("${backing-services.report-server.base-uri:http://report-server}") String reportServerURI,
                                      NetworkStoreService networkStoreService, NetworkModificationRepository networkModificationRepository,
                                      @Lazy EquipmentInfosService equipmentInfosService, ModificationGroupRepository modificationGroupRepository,
                                      ModificationRepository modificationRepository, ModificationApplicator modificationApplicator) {
        this.networkStoreService = networkStoreService;
        this.networkModificationRepository = networkModificationRepository;
        this.equipmentInfosService = equipmentInfosService;
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
        this.modificationApplicator = modificationApplicator;

        RestTemplateBuilder restTemplateBuilder = new RestTemplateBuilder();
        reportServerRest = restTemplateBuilder.build();
        reportServerRest.setUriTemplateHandler(new DefaultUriBuilderFactory(reportServerURI));

        objectMapper = Jackson2ObjectMapperBuilder.json().build();
        objectMapper.registerModule(new ReporterModelJsonModule());
        objectMapper.setInjectableValues(new InjectableValues.Std().addValue(ReporterModelDeserializer.DICTIONARY_VALUE_ID, null));
    }

    private List<ModificationInfos> execApplyGroovyScript(NetworkStoreListener listener,
                                                          String groovyScript,
                                                          UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Apply groovy script";
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the groovy script modification entity to the listener
            listener.storeGroovyScriptModification(groovyScript);
        }, GROOVY_SCRIPT_ERROR, reportUuid, reporter, subReporter);
    }

    public List<ModificationInfos> applyGroovyScript(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String groovyScript) {
        assertGroovyScriptNotEmpty(groovyScript);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execApplyGroovyScript(listener, groovyScript, reportUuid);
    }

    public List<EquipmentAttributeModificationInfos> changeSwitchState(UUID networkUuid, String variantId, GroupAndReportInfos groupAndReportInfos, String switchId, boolean open) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupAndReportInfos.getGroupUuid(), networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder().equipmentType(IdentifiableType.SWITCH).equipmentId(switchId).equipmentAttributeName("open").equipmentAttributeValue(open).build();
        return self.handleModification(modificationInfos, listener, groupAndReportInfos).stream().map(EquipmentAttributeModificationInfos.class::cast).collect(Collectors.toList());
    }

    public List<UUID> getModificationGroups() {
        return networkModificationRepository.getModificationGroupsUuids();
    }

    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        return networkModificationRepository.getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound);
    }

    public List<ModificationInfos> getModification(UUID modificationUuid) {
        return networkModificationRepository.getModifications(List.of(modificationUuid));
    }

    public void createModificationGroup(UUID sourceGroupUuid, UUID groupUuid, UUID reportUuid) {
        List<ModificationEntity> entities = networkModificationRepository.getModificationsEntities(List.of(sourceGroupUuid))
            .stream()
            .map(networkModificationRepository::getModificationEntityEagerly)
            .collect(Collectors.toList());
        entities.forEach(ModificationEntity::setIdsToNull);
        networkModificationRepository.saveModifications(groupUuid, entities);
        if (!entities.isEmpty()) {
            ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
            sendReport(reportUuid, reporter);
        }
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();
        return terminal1Disconnected && terminal2Disconnected;
    }

    public List<ModificationInfos> changeLineStatus(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String lineId, String action) {
        assertBranchActionValid(action);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execChangeLineStatus(listener, lineId, BranchStatusModificationInfos.ActionType.valueOf(action.toUpperCase()), reportUuid);
    }

    private List<ModificationInfos> execChangeLineStatus(NetworkStoreListener listener, String lineId, BranchStatusModificationInfos.ActionType action, UUID reportUuid) {
        switch (action) {
            case LOCKOUT:
                return execLockoutLine(listener, lineId, reportUuid);
            case TRIP:
                return execTripLine(listener, lineId, reportUuid);
            case SWITCH_ON:
                return execSwitchOnLine(listener, lineId, reportUuid);
            case ENERGISE_END_ONE:
                return execEnergiseLineEnd(listener, lineId, Branch.Side.ONE, reportUuid);
            case ENERGISE_END_TWO:
                return execEnergiseLineEnd(listener, lineId, Side.TWO, reportUuid);
            default:
                throw NetworkModificationException.createBranchActionTypeUnsupported(action);
        }
    }

    private List<ModificationInfos> execLockoutLine(NetworkStoreListener listener, String lineId, UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Lockout line " + lineId;
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
                if (listener.getNetwork().getLine(lineId) == null) {
                    throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
                }
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
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
                }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, BranchStatusModificationInfos.ActionType.LOCKOUT);
            }, MODIFICATION_ERROR, reportUuid, reporter, subReporter
        );
    }

    private List<ModificationInfos> execTripLine(NetworkStoreListener listener, String lineId, UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Trip line " + lineId;
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
                if (listener.getNetwork().getLine(lineId) == null) {
                    throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
                }
                if (listener.isApplyModifications()) {
                    var trip = new BranchTripping(lineId);
                    var switchToDisconnect = new HashSet<Switch>();
                    var terminalsToDisconnect = new HashSet<Terminal>();
                    var traversedTerminals = new HashSet<Terminal>();
                    trip.traverse(network, switchToDisconnect, terminalsToDisconnect, traversedTerminals);

                    switchToDisconnect.forEach(sw -> sw.setOpen(true));
                    terminalsToDisconnect.forEach(Terminal::disconnect);

                    subReporter.report(Report.builder()
                        .withKey("tripLineApplied")
                        .withDefaultMessage("Line ${id} (id) : trip applied")
                        .withValue("id", lineId)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

                    traversedTerminals.stream().map(t -> network.getLine(t.getConnectable().getId())).filter(Objects::nonNull)
                        .forEach(b -> b.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add());
                }
                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, BranchStatusModificationInfos.ActionType.TRIP);

        }, MODIFICATION_ERROR, reportUuid, reporter, subReporter
        );
    }

    private List<ModificationInfos> execEnergiseLineEnd(NetworkStoreListener listener,
                                                        String lineId,
                                                        Branch.Side side,
                                                        UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Energise line " + lineId;
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.getNetwork().getLine(lineId) == null) {
                throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
            }
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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, side == Branch.Side.ONE ? BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE : BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO);
            }, MODIFICATION_ERROR, reportUuid, reporter, subReporter
        );
    }

    private List<ModificationInfos> execSwitchOnLine(NetworkStoreListener listener, String lineId, UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Switch on line " + lineId;
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.getNetwork().getLine(lineId) == null) {
                throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
            }
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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

                // add the branch status modification entity to the listener
                listener.storeBranchStatusModification(lineId, BranchStatusModificationInfos.ActionType.SWITCH_ON);
            }, MODIFICATION_ERROR, reportUuid, reporter, subReporter
        );
    }

    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        networkModificationRepository.deleteModificationGroup(groupUuid, errorOnGroupNotFound);
    }

    public List<ModificationInfos> doAction(NetworkStoreListener listener, Runnable action,
                                            NetworkModificationException.Type typeIfError,
                                            UUID reportUuid, ReporterModel reporter,
                                            Reporter subReporter) {
        try {
            action.run();
            if (!listener.isBuild()) {
                saveModifications(listener);
            }
            return listener.isApplyModifications() ? listener.getModifications() : Collections.emptyList();
        } catch (PowsyblException e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(typeIfError, e);
            subReporter.report(Report.builder()
                .withKey(typeIfError.name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
            if (!listener.isBuild()) {
                throw exc;
            } else {
                return Collections.emptyList();
            }
        } catch (Exception e) {
            if (!listener.isBuild()) {
                throw new NetworkModificationException(typeIfError, e);
            } else {
                throw e;
            }
        } finally {
            if (listener.isApplyModifications()) {
                sendReport(reportUuid, reporter);
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

    private ModificationNetworkInfos getNetworkModificationInfos(UUID networkUuid, String variantId) {
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
    }

    private void assertGroovyScriptNotEmpty(String groovyScript) {
        if (StringUtils.isBlank(groovyScript)) {
            throw new NetworkModificationException(GROOVY_SCRIPT_EMPTY);
        }
    }

    private void assertBranchActionValid(String action) {
        if (StringUtils.isBlank(action)) {
            throw new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY);
        }
        try {
            BranchStatusModificationInfos.ActionType.valueOf(action.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw NetworkModificationException.createBranchActionTypeUnknown(action);
        }
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

    public void updateGeneratorCreation(GeneratorCreationInfos generatorCreationInfos, UUID modificationUuid) {
        assertGeneratorCreationInfosNotEmpty(generatorCreationInfos);

        Optional<ModificationEntity> generatorModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!generatorModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator creation not found");
        }

        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createGeneratorEntity(
                generatorCreationInfos.getEquipmentId(),
                generatorCreationInfos.getEquipmentName(),
                generatorCreationInfos.getEnergySource(),
                generatorCreationInfos.getVoltageLevelId(),
                generatorCreationInfos.getBusOrBusbarSectionId(),
                generatorCreationInfos.getMinActivePower(),
                generatorCreationInfos.getMaxActivePower(),
                generatorCreationInfos.getRatedNominalPower(),
                generatorCreationInfos.getActivePowerSetpoint(),
                generatorCreationInfos.getReactivePowerSetpoint(),
                generatorCreationInfos.isVoltageRegulationOn(),
                generatorCreationInfos.getVoltageSetpoint(),
                generatorCreationInfos.getMarginalCost(),
                generatorCreationInfos.getMinimumReactivePower(),
                generatorCreationInfos.getMaximumReactivePower(),
                generatorCreationInfos.getParticipate(),
                generatorCreationInfos.getDroop(),
                generatorCreationInfos.getTransientReactance(),
                generatorCreationInfos.getStepUpTransformerReactance(),
                generatorCreationInfos.getRegulatingTerminalId(),
                generatorCreationInfos.getRegulatingTerminalType(),
                generatorCreationInfos.getRegulatingTerminalVlId(),
                generatorCreationInfos.getReactiveCapabilityCurve(),
                toEmbeddablePoints(generatorCreationInfos.getPoints()));

        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(generatorModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    public List<EquipmentModificationInfos> createLoad(UUID networkUuid, String variantId, GroupAndReportInfos groupAndReportInfos, LoadCreationInfos loadCreationInfos) {
        assertLoadCreationInfosNotEmpty(loadCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupAndReportInfos.getGroupUuid(), networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return self.handleModification(loadCreationInfos, listener, groupAndReportInfos).stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    @Transactional
    // Generic form
    public List<ModificationInfos> handleModification(ModificationInfos modificationInfos, NetworkStoreListener listener, GroupAndReportInfos groupAndReportIds) {
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        List<ModificationInfos> networkModifications = List.of();
        try {
            if (listener.isApplyModifications()) {
                networkModifications = modificationApplicator.apply(modificationInfos, reporter, listener);
            }
            if (!listener.isBuild()) {
                saveModifications(listener, groupAndReportIds.getGroupUuid(), modificationInfos.toEntity());
            }
            return networkModifications;
        } finally {
            if (listener.isApplyModifications()) {
                sendReport(groupAndReportIds.getReportUuid(), reporter);
            }
        }
    }

    private void saveModifications(NetworkStoreListener listener, UUID groupUuid, ModificationEntity modificationEntity) {
        networkModificationRepository.saveModifications(groupUuid, List.of(modificationEntity));
        if (listener.isApplyModifications()) {
            networkStoreService.flush(listener.getNetwork());
        }
    }

    @Transactional
    // Generic form
    public void updateModification(ModificationInfos modificationInfos, UUID modificationUuid) {
        ModificationEntity modificationEntity = this.modificationRepository.findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)));

        modificationEntity.update(modificationInfos);
    }

    public void updateLoadCreation(LoadCreationInfos loadCreationInfos, UUID modificationUuid) {
        assertLoadCreationInfosNotEmpty(loadCreationInfos);
        self.updateModification(loadCreationInfos, modificationUuid);
    }

    private void assertLoadCreationInfosNotEmpty(LoadCreationInfos loadCreationInfos) {
        if (loadCreationInfos == null) {
            throw new NetworkModificationException(CREATE_LOAD_ERROR, "Missing required attributes to create the load");
        }
    }

    private void modifyLoad(Load load, LoadModificationInfos loadModificationInfos, Reporter subReporter) {
        subReporter.report(Report.builder()
            .withKey("loadModification")
            .withDefaultMessage("Load with id=${id} modified :")
            .withValue("id", loadModificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());

        applyElementaryModifications(load::setName, load::getNameOrId, loadModificationInfos.getEquipmentName(), subReporter, "Name");
        applyElementaryModifications(load::setLoadType, load::getLoadType, loadModificationInfos.getLoadType(), subReporter, "Type");
        applyElementaryModifications(load::setP0, load::getP0, loadModificationInfos.getActivePower(), subReporter, "Active power");
        applyElementaryModifications(load::setQ0, load::getQ0, loadModificationInfos.getReactivePower(), subReporter, "Reactive power");

        // TODO connectivity modification
    }

    public void updateLoadModification(LoadModificationInfos loadModificationInfos, UUID modificationUuid) {
        assertEquipmentModificationInfosOk(loadModificationInfos, MODIFY_LOAD_ERROR);

        Optional<ModificationEntity> loadModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!loadModificationEntity.isPresent()) {
            throw new NetworkModificationException(MODIFY_LOAD_ERROR, "Load modification not found");
        }
        EquipmentModificationEntity updatedEntity = this.networkModificationRepository.createLoadModificationEntity(
                loadModificationInfos.getEquipmentId(),
                loadModificationInfos.getEquipmentName(),
                loadModificationInfos.getLoadType(),
                loadModificationInfos.getVoltageLevelId(),
                loadModificationInfos.getBusOrBusbarSectionId(),
                loadModificationInfos.getActivePower(),
                loadModificationInfos.getReactivePower());
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(loadModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private List<EquipmentModificationInfos> execModifyLoad(NetworkStoreListener listener,
                                                            LoadModificationInfos loadModificationInfos,
                                                            UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Load modification " + loadModificationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                try {
                    Load load = network.getLoad(loadModificationInfos.getEquipmentId());
                    if (load == null) {
                        throw new NetworkModificationException(LOAD_NOT_FOUND, "Load " + loadModificationInfos.getEquipmentId() + " does not exist in network");
                    }

                    // modify the load in the network
                    modifyLoad(load, loadModificationInfos, subReporter);
                } catch (NetworkModificationException exc) {
                    subReporter.report(Report.builder()
                            .withKey("loadModification")
                            .withDefaultMessage(exc.getMessage())
                            .withValue("id", loadModificationInfos.getEquipmentId())
                            .withSeverity(TypedValue.ERROR_SEVERITY)
                            .build());
                }
            }

            // add the load modification entity to the listener
            listener.storeLoadModification(loadModificationInfos);
        }, MODIFY_LOAD_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
                .collect(Collectors.toList());
    }

    private static <T> void applyElementaryModifications(Consumer<T> setter, Supplier<T> getter,
                                                         AttributeModification<T> modification,
                                                         Reporter subReporter, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            subReporter.report(Report.builder()
                .withKey("Modification" + fieldName)
                .withDefaultMessage("    ${fieldName} : ${oldValue} -> ${newValue}")
                .withValue("fieldName", fieldName)
                .withValue("oldValue", oldValue.toString())
                .withValue("newValue", newValue.toString())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        }
    }

    private void modifyGenerator(Generator generator, GeneratorModificationInfos modificationInfos, Reporter subReporter) {
        subReporter.report(Report.builder()
            .withKey("generatorModification")
            .withDefaultMessage("Generator with id=${id} modified :")
            .withValue("id", modificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());

        applyElementaryModifications(generator::setName, generator::getNameOrId, modificationInfos.getEquipmentName(), subReporter, "Name");
        applyElementaryModifications(generator::setEnergySource, generator::getEnergySource, modificationInfos.getEnergySource(), subReporter, "Energy source");
        applyElementaryModifications(generator::setMinP, generator::getMinP, modificationInfos.getMinActivePower(), subReporter, "Min active power");
        applyElementaryModifications(generator::setMaxP, generator::getMaxP, modificationInfos.getMaxActivePower(), subReporter, "Max active power");
        applyElementaryModifications(generator::setRatedS, generator::getRatedS, modificationInfos.getRatedNominalPower(), subReporter, "Rated nominal power");
        applyElementaryModifications(generator::setTargetP, generator::getTargetP, modificationInfos.getActivePowerSetpoint(), subReporter, "Active power set point");
        applyElementaryModifications(generator::setTargetQ, generator::getTargetQ, modificationInfos.getReactivePowerSetpoint(), subReporter, "Reactive power set point");
        applyElementaryModifications(generator::setTargetV, generator::getTargetV, modificationInfos.getVoltageSetpoint(), subReporter, "Voltage set point");
        applyElementaryModifications(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn, modificationInfos.getVoltageRegulationOn(), subReporter, "Voltage regulation on");

        // TODO connectivity modification
    }

    public List<EquipmentModificationInfos> modifyLoad(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, LoadModificationInfos loadModificationInfos) {
        assertEquipmentModificationInfosOk(loadModificationInfos, MODIFY_LOAD_ERROR);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execModifyLoad(listener, loadModificationInfos, reportUuid);
    }

    private List<EquipmentModificationInfos> execModifyGenerator(NetworkStoreListener listener,
                                                            GeneratorModificationInfos generatorModificationInfos,
                                                            UUID repordId
    ) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Generator modification " + generatorModificationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                try {
                    Generator generator = network.getGenerator(generatorModificationInfos.getEquipmentId());
                    if (generator == null) {
                        throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + generatorModificationInfos.getEquipmentId() + " does not exist in network");
                    }

                    // modify the generator in the network
                    modifyGenerator(generator, generatorModificationInfos, subReporter);
                } catch (NetworkModificationException exc) {
                    subReporter.report(Report.builder()
                        .withKey("generatorModification")
                        .withDefaultMessage(exc.getMessage())
                        .withValue("id", generatorModificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
                }
            }

            // add the generator modification entity to the listener
            listener.storeGeneratorModification(generatorModificationInfos);
        }, MODIFY_GENERATOR_ERROR, repordId, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> modifyGenerator(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, GeneratorModificationInfos generatorModificationInfo) {
        assertEquipmentModificationInfosOk(generatorModificationInfo, MODIFY_GENERATOR_ERROR);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execModifyGenerator(listener, generatorModificationInfo, reportUuid);
    }

    private void assertEquipmentModificationInfosOk(BasicEquipmentModificationInfos equipmentModificationInfos, NetworkModificationException.Type type) {
        if (equipmentModificationInfos == null || equipmentModificationInfos.getEquipmentId() == null) {
            throw new NetworkModificationException(type, "Missing required attributes to modify the equipment");
        }
    }

    private List<EquipmentDeletionInfos> execDeleteEquipment(NetworkStoreListener listener,
                                                             String equipmentType,
                                                             String equipmentId,
                                                             UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Equipment deletion " + equipmentId;
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, equipmentType, equipmentId);
                if (identifiable == null) {
                    throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + equipmentId + " not found or of bad type");
                }

                if (identifiable instanceof Connectable) {
                    ((Connectable) identifiable).remove(true);
                } else if (identifiable instanceof HvdcLine) {
                    ((HvdcLine) identifiable).remove();
                } else if (identifiable instanceof VoltageLevel) {
                    ((VoltageLevel) identifiable).remove();
                } else if (identifiable instanceof Substation) {
                    ((Substation) identifiable).remove();
                }

                subReporter.report(Report.builder()
                    .withKey("equipmentDeleted")
                    .withDefaultMessage("equipment of type=${type} and id=${id} deleted")
                    .withValue("type", equipmentType)
                    .withValue("id", equipmentId)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the equipment deletion entity to the listener
            listener.storeEquipmentDeletion(equipmentId, equipmentType);
        }, DELETE_EQUIPMENT_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentDeletionInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentDeletionInfos> deleteEquipment(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String equipmentType, String equipmentId) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execDeleteEquipment(listener, equipmentType, equipmentId, reportUuid);
    }

    private void sendReport(UUID reportUuid, ReporterModel reporter) {
        var headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        var resourceUrl = DELIMITER + REPORT_API_VERSION + DELIMITER + "reports" + DELIMITER + reportUuid;
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
        int nodeNum = ModificationUtils.getInstance().createNodeBreakerCellSwitches(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId(),
            generatorCreationInfos.getEquipmentId(),
            generatorCreationInfos.getEquipmentName());

        Terminal terminal = getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                                                        generatorCreationInfos.getRegulatingTerminalId(),
                                                        generatorCreationInfos.getRegulatingTerminalType(),
                                                        generatorCreationInfos.getRegulatingTerminalVlId());

        // creating the generator
        Generator generator = voltageLevel.newGenerator()
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

        if (terminal != null) {
            generator.setRegulatingTerminal(terminal);
        }

        Boolean participate = generatorCreationInfos.getParticipate();

        if (generatorCreationInfos.getMarginalCost() != null) {
            generator.newExtension(GeneratorStartupAdderImpl.class).withMarginalCost(generatorCreationInfos.getMarginalCost()).add();
        }

        if (generatorCreationInfos.getParticipate() != null && generatorCreationInfos.getDroop() != null) {
            generator.newExtension(ActivePowerControlAdder.class).withParticipate(participate)
                    .withDroop(generatorCreationInfos.getDroop())
                    .add();
        }

        if (generatorCreationInfos.getTransientReactance() != null && generatorCreationInfos.getStepUpTransformerReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(generatorCreationInfos.getTransientReactance())
                    .withStepUpTransformerX(generatorCreationInfos.getStepUpTransformerReactance())
                    .add();
        }

        if (generatorCreationInfos.getPoints() != null) {
            ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
            generatorCreationInfos.getPoints()
                    .forEach(point -> adder.beginPoint()
                            .setMaxQ(point.getQmaxP())
                            .setMinQ(point.getQminP())
                            .setP(point.getP())
                            .endPoint());
            adder.add();
        }

        if (generatorCreationInfos.getMinimumReactivePower() != null && generatorCreationInfos.getMaximumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(generatorCreationInfos.getMinimumReactivePower())
                    .setMaxQ(generatorCreationInfos.getMaximumReactivePower())
                    .add();
        }

        return generator;
    }

    private Generator createGeneratorInBusBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos) {
        Bus bus = getBusBreakerBus(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId());

        Terminal terminal = getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                                                        generatorCreationInfos.getRegulatingTerminalId(),
                                                        generatorCreationInfos.getRegulatingTerminalType(),
                                                        generatorCreationInfos.getRegulatingTerminalVlId());

        // creating the generator
        Generator generator = voltageLevel.newGenerator()
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

        if (terminal != null) {
            generator.setRegulatingTerminal(terminal);
        }

        if (generatorCreationInfos.getTransientReactance() != null && generatorCreationInfos.getStepUpTransformerReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class).withDirectTransX(generatorCreationInfos.getTransientReactance())
                    .withStepUpTransformerX(generatorCreationInfos.getStepUpTransformerReactance())
                    .add();
        }

        if (generatorCreationInfos.getMarginalCost() != null) {
            generator.newExtension(GeneratorStartupAdder.class).withMarginalCost(generatorCreationInfos.getMarginalCost()).add();
        }

        if (generatorCreationInfos.getParticipate() != null && generatorCreationInfos.getDroop() != null) {
            generator.newExtension(ActivePowerControlAdder.class).withParticipate(generatorCreationInfos.getParticipate())
                    .withDroop(generatorCreationInfos.getDroop())
                    .add();
        }

        if (generatorCreationInfos.getMaximumReactivePower() != null && generatorCreationInfos.getMinimumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(generatorCreationInfos.getMinimumReactivePower())
                    .setMaxQ(generatorCreationInfos.getMaximumReactivePower())
                    .add();
        }

        if (generatorCreationInfos.getPoints() != null) {
            ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
            generatorCreationInfos.getPoints()
                    .forEach(point -> adder.beginPoint()
                            .setMaxQ(point.getQmaxP())
                            .setMinQ(point.getQminP())
                            .setP(point.getP())
                            .endPoint());
            adder.add();
        }

        return generator;
    }

    private List<EquipmentModificationInfos> execCreateGenerator(NetworkStoreListener listener,
                                                                 GeneratorCreationInfos generatorCreationInfos,
                                                                 UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Generator creation " + generatorCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the generator creation entity to the listener
            listener.storeGeneratorCreation(generatorCreationInfos);
        }, CREATE_GENERATOR_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> createGenerator(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, GeneratorCreationInfos generatorCreationInfos) {
        assertGeneratorCreationInfosNotEmpty(generatorCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateGenerator(listener, generatorCreationInfos, reportUuid);
    }

    private void assertGeneratorCreationInfosNotEmpty(GeneratorCreationInfos generatorCreationInfos) {
        if (generatorCreationInfos == null) {
            throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Missing required attributes to create the generator");
        }
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
            int nodeNum = ModificationUtils.getInstance().createNodeBreakerCellSwitches(voltageLevel,
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

    public void updateLineCreation(LineCreationInfos lineCreationInfos, UUID modificationUuid) {
        assertLineCreationInfosNotEmpty(lineCreationInfos);

        Optional<ModificationEntity> lineModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!lineModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_LINE_ERROR, "Line creation not found");
        }

        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createLineEntity(
                lineCreationInfos.getEquipmentId(),
                lineCreationInfos.getEquipmentName(),
                lineCreationInfos.getSeriesResistance(),
                lineCreationInfos.getSeriesReactance(),
                lineCreationInfos.getShuntConductance1(),
                lineCreationInfos.getShuntSusceptance1(),
                lineCreationInfos.getShuntConductance2(),
                lineCreationInfos.getShuntSusceptance2(),
                lineCreationInfos.getVoltageLevelId1(),
                lineCreationInfos.getBusOrBusbarSectionId1(),
                lineCreationInfos.getVoltageLevelId2(),
                lineCreationInfos.getBusOrBusbarSectionId2(),
                lineCreationInfos.getCurrentLimits1().getPermanentLimit(),
                lineCreationInfos.getCurrentLimits2().getPermanentLimit());
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(lineModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private List<EquipmentModificationInfos> execCreateLine(NetworkStoreListener listener,
                                                            LineCreationInfos lineCreationInfos,
                                                            UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Line creation " + lineCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the line creation entity to the listener
            listener.storeLineCreation(lineCreationInfos);
        }, CREATE_LINE_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> createLine(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, LineCreationInfos lineCreationInfos) {
        assertLineCreationInfosNotEmpty(lineCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateLine(listener, lineCreationInfos, reportUuid);
    }

    private void assertLineCreationInfosNotEmpty(LineCreationInfos lineCreationInfos) {
        if (lineCreationInfos == null) {
            throw new NetworkModificationException(CREATE_LINE_ERROR, "Missing required attributes to create the line");
        }
    }

    private List<EquipmentModificationInfos> execCreateTwoWindingsTransformer(NetworkStoreListener listener,
                                                                              TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos,
                                                                              UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Two windings transformer creation " + twoWindingsTransformerCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the 2wt creation entity to the listener
            listener.storeTwoWindingsTransformerCreation(twoWindingsTransformerCreationInfos);
        }, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> createTwoWindingsTransformer(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        assertTwoWindingsTransformerCreationInfosNotEmpty(twoWindingsTransformerCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateTwoWindingsTransformer(listener, twoWindingsTransformerCreationInfos, reportUuid);
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

    public void updateTwoWindingsTransformerCreation(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, UUID modificationUuid) {
        assertTwoWindingsTransformerCreationInfosNotEmpty(twoWindingsTransformerCreationInfos);
        Optional<ModificationEntity> twoWindingsTransformerModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!twoWindingsTransformerModificationEntity.isPresent()) {
            throw  new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Two windings transformer creation not found");
        }
        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createTwoWindingsTransformerEntity(
                twoWindingsTransformerCreationInfos.getEquipmentId(),
                twoWindingsTransformerCreationInfos.getEquipmentName(),
                twoWindingsTransformerCreationInfos.getSeriesResistance(),
                twoWindingsTransformerCreationInfos.getSeriesReactance(),
                twoWindingsTransformerCreationInfos.getMagnetizingConductance(),
                twoWindingsTransformerCreationInfos.getMagnetizingSusceptance(),
                twoWindingsTransformerCreationInfos.getRatedVoltage1(),
                twoWindingsTransformerCreationInfos.getRatedVoltage2(),
                twoWindingsTransformerCreationInfos.getVoltageLevelId1(),
                twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId1(),
                twoWindingsTransformerCreationInfos.getVoltageLevelId2(),
                twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId2(),
                twoWindingsTransformerCreationInfos.getCurrentLimits1() != null ? twoWindingsTransformerCreationInfos.getCurrentLimits1().getPermanentLimit() : null,
                twoWindingsTransformerCreationInfos.getCurrentLimits2() != null ? twoWindingsTransformerCreationInfos.getCurrentLimits2().getPermanentLimit() : null
        );
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(twoWindingsTransformerModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private void assertTwoWindingsTransformerCreationInfosNotEmpty(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        if (twoWindingsTransformerCreationInfos == null) {
            throw new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Missing required attributes to create the two windings transformer");
        }
    }

    private List<EquipmentModificationInfos> execCreateSubstation(NetworkStoreListener listener,
                                                                  SubstationCreationInfos substationCreationInfos,
                                                                  UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Substation creation " + substationCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the substation creation entity to the listener
            listener.storeSubstationCreation(substationCreationInfos);
        }, CREATE_SUBSTATION_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> createSubstation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, SubstationCreationInfos substationCreationInfos) {
        assertSubstationCreationInfosNotEmpty(substationCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateSubstation(listener, substationCreationInfos, reportUuid);
    }

    public void updateSubstationCreation(SubstationCreationInfos substationCreationInfos, UUID modificationUuid) {
        Optional<ModificationEntity> substationModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!substationModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Substation creation not found");
        }

        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createSubstationEntity(substationCreationInfos.getEquipmentId(), substationCreationInfos.getEquipmentName(), substationCreationInfos.getSubstationCountry());
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(substationModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private void assertSubstationCreationInfosNotEmpty(SubstationCreationInfos substationCreationInfos) {
        if (substationCreationInfos == null) {
            throw new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Missing required attributes to create the substation");
        }
    }

    private List<EquipmentModificationInfos> execCreateVoltageLevel(NetworkStoreListener listener, VoltageLevelCreationInfos voltageLevelCreationInfos,
                                                                    UUID reportUuid) {

        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "VoltageLevel creation " + voltageLevelCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                ModificationUtils.getInstance().createVoltageLevelAction(voltageLevelCreationInfos, subReporter, network);
            }
            listener.storeVoltageLevelCreation(voltageLevelCreationInfos);
        }, CREATE_VOLTAGE_LEVEL_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> createVoltageLevel(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid,
           VoltageLevelCreationInfos voltageLevelCreationInfos) {
        assertVoltageLevelCreationInfosNotEmpty(voltageLevelCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid,
                networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateVoltageLevel(listener, voltageLevelCreationInfos, reportUuid);
    }

    public void updateVoltageLevelCreation(VoltageLevelCreationInfos voltageLevelCreationInfos, UUID modificationUuid) {
        assertVoltageLevelCreationInfosNotEmpty(voltageLevelCreationInfos);

        Optional<ModificationEntity> voltageLevelModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!voltageLevelModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level creation not found");
        }

        List<BusbarSectionCreationEmbeddable> busbarSections = voltageLevelCreationInfos.getBusbarSections().stream().map(bbsi ->
                new BusbarSectionCreationEmbeddable(bbsi.getId(), bbsi.getName(), bbsi.getVertPos(), bbsi.getHorizPos())
        ).collect(Collectors.toList());
        List<BusbarConnectionCreationEmbeddable> busbarConnections = voltageLevelCreationInfos.getBusbarConnections().stream().map(cnxi ->
                new BusbarConnectionCreationEmbeddable(cnxi.getFromBBS(), cnxi.getToBBS(), cnxi.getSwitchKind())
        ).collect(Collectors.toList());

        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createVoltageLevelEntity(
                voltageLevelCreationInfos.getEquipmentId(),
                voltageLevelCreationInfos.getEquipmentName(),
                voltageLevelCreationInfos.getNominalVoltage(),
                voltageLevelCreationInfos.getSubstationId(),
                busbarSections,
                busbarConnections);
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(voltageLevelModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private void assertVoltageLevelCreationInfosNotEmpty(VoltageLevelCreationInfos voltageLevelCreationInfos) {
        if (voltageLevelCreationInfos == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Missing required attributes to create the voltage level");
        }
    }

    public Network cloneNetworkVariant(UUID networkUuid, String originVariantId, String destinationVariantId) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid);
            network.addListener(new NetworkVariantsListener(network, networkUuid, equipmentInfosService));
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
    }

    public List<ModificationInfos> applyModifications(Network network, UUID networkUuid, BuildInfos buildInfos) {
        // Apply all modifications belonging to the modification groups uuids in buildInfos
        List<ModificationInfos> allModificationsInfos = new ArrayList<>();
        NetworkStoreListener listener = NetworkStoreListener.create(network,
            networkUuid,
            null,
            networkModificationRepository,
            equipmentInfosService,
            true,
            true);

        Stream<GroupAndReportInfos> groupsAndReporstIds = Streams.zip(buildInfos.getModificationGroupUuids().stream(), buildInfos.getReportUuids().stream(),
            GroupAndReportInfos::new);
        groupsAndReporstIds.forEach(groupAndReportIds -> {
            if (modificationGroupRepository.findById(groupAndReportIds.getGroupUuid()).isPresent()) { // May not exist
                networkModificationRepository
                    .getModificationsInfos(List.of(groupAndReportIds.getGroupUuid()))
                    .forEach(infos -> applyModification(allModificationsInfos, listener, buildInfos.getModificationsToExclude(), groupAndReportIds, infos));
            }
        });

        // flushing network (only once at the end)
        networkStoreService.flush(listener.getNetwork());

        return allModificationsInfos;
    }

    private void applyModification(List<ModificationInfos> allModificationsInfos, NetworkStoreListener listener,
        Set<UUID> modificationsToExclude, GroupAndReportInfos groupAndReportIds, ModificationInfos infos) {
        UUID reportUuid = groupAndReportIds.getReportUuid();
        try {
            if (modificationsToExclude.contains(infos.getUuid())) {
                return;
            }
            ModificationType type = infos.getType();

            switch (type) {
                case EQUIPMENT_ATTRIBUTE_MODIFICATION:
                case LOAD_CREATION:
                case LINE_SPLIT_WITH_VOLTAGE_LEVEL:
                    // Generic form
                    allModificationsInfos.addAll(self.handleModification(infos, listener, groupAndReportIds));
                    break;

                case LOAD_MODIFICATION: {
                    LoadModificationInfos loadModificationInfos = (LoadModificationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execModifyLoad(listener, loadModificationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case GENERATOR_CREATION: {
                    GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execCreateGenerator(listener, generatorCreationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case GENERATOR_MODIFICATION: {
                    var generatorModificationInfos = (GeneratorModificationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execModifyGenerator(listener, generatorModificationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case LINE_CREATION: {
                    LineCreationInfos lineCreationInfos = (LineCreationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execCreateLine(listener, lineCreationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case TWO_WINDINGS_TRANSFORMER_CREATION: {
                    TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execCreateTwoWindingsTransformer(listener, twoWindingsTransformerCreationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case EQUIPMENT_DELETION: {
                    EquipmentDeletionInfos deletionInfos = (EquipmentDeletionInfos) infos;
                    List<EquipmentDeletionInfos> modificationInfos = execDeleteEquipment(listener, deletionInfos.getEquipmentType(), deletionInfos.getEquipmentId(), reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case GROOVY_SCRIPT: {
                    GroovyScriptModificationInfos groovyModificationInfos = (GroovyScriptModificationInfos) infos;
                    List<ModificationInfos> modificationInfos = execApplyGroovyScript(listener, groovyModificationInfos.getScript(), reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case SUBSTATION_CREATION: {
                    SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execCreateSubstation(listener, substationCreationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case VOLTAGE_LEVEL_CREATION: {
                    VoltageLevelCreationInfos voltageLevelCreationInfos = (VoltageLevelCreationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execCreateVoltageLevel(listener, voltageLevelCreationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case BRANCH_STATUS: {
                    BranchStatusModificationInfos branchStatusModificationInfos = (BranchStatusModificationInfos) infos;
                    List<ModificationInfos> modificationInfos = execChangeLineStatus(listener, branchStatusModificationInfos.getEquipmentId(), branchStatusModificationInfos.getAction(), reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case SHUNT_COMPENSATOR_CREATION: {
                    ShuntCompensatorCreationInfos shuntCompensatorCreationInfos = (ShuntCompensatorCreationInfos) infos;
                    List<EquipmentModificationInfos> modificationInfos = execCreateShuntCompensator(listener, shuntCompensatorCreationInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                case LINE_ATTACH_TO_VOLTAGE_LEVEL: {
                    LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos = (LineAttachToVoltageLevelInfos) infos;
                    List<ModificationInfos> modificationInfos = execLineAttachToVoltageLevel(listener, lineAttachToVoltageLevelInfos, reportUuid);
                    allModificationsInfos.addAll(modificationInfos);
                }
                break;

                default:
            }
        } catch (PowsyblException e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(MODIFICATION_ERROR, e);
            ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, "Building node");
            reporter.report(Report.builder()
                .withKey(MODIFICATION_ERROR.name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
            sendReport(reportUuid, reporter);
        }
    }

    /*
    ** Build variant : sending message to rabbitmq
     */
    public void buildVariant(UUID networkUuid, BuildInfos buildInfos, String receiver) {
        sendRunBuildMessage(new BuildExecContext(networkUuid, buildInfos, receiver).toMessage(objectMapper));
    }

    private void sendRunBuildMessage(Message<String> message) {
        RUN_MESSAGE_LOGGER.debug("Sending message : {}", message);
        publisher.send("publishBuild-out-0", message);
    }

    public void stopBuild(String receiver) {
        sendCancelBuildMessage(new BuildCancelContext(receiver).toMessage());
    }

    private void sendCancelBuildMessage(Message<String> message) {
        CANCEL_MESSAGE_LOGGER.debug("Sending message : {}", message);
        publisher.send("publishCancelBuild-out-0", message);
    }

    public void deleteModifications(UUID groupUuid, Set<UUID> modificationsUuids) {
        if (networkModificationRepository.deleteModifications(groupUuid, modificationsUuids) == 0) {
            throw new NetworkModificationException(MODIFICATION_NOT_FOUND);
        }
    }

    private void assertShuntCompensatorCreationInfosNotEmpty(ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        if (shuntCompensatorCreationInfos == null) {
            throw new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, "Missing required attributes to create the shunt Compensator");
        }
    }

    private void createShuntCompensator(VoltageLevel voltageLevel, ShuntCompensatorCreationInfos shuntCompensatorInfos, boolean isNodeBreaker) {
        // creating the shunt compensator
        var shunt = voltageLevel.newShuntCompensator()
            .setId(shuntCompensatorInfos.getEquipmentId())
            .setName(shuntCompensatorInfos.getEquipmentName())
            .setSectionCount(shuntCompensatorInfos.getCurrentNumberOfSections());

        /* connect it !*/
        if (isNodeBreaker) {
            // create cell switches
            int nodeNum = ModificationUtils.getInstance().createNodeBreakerCellSwitches(voltageLevel, shuntCompensatorInfos.getBusOrBusbarSectionId(),
                shuntCompensatorInfos.getEquipmentId(),
                shuntCompensatorInfos.getEquipmentName());
            shunt.setNode(nodeNum);
        } else {
            Bus bus = getBusBreakerBus(voltageLevel, shuntCompensatorInfos.getBusOrBusbarSectionId());
            shunt.setBus(bus.getId())
                 .setConnectableBus(bus.getId());
        }

        /* when we create non linear shunt, this is where we branch ;) */
        shunt.newLinearModel()
            .setBPerSection(shuntCompensatorInfos.getSusceptancePerSection())
            .setMaximumSectionCount(shuntCompensatorInfos.getMaximumNumberOfSections()).add();

        shunt.add();
    }

    public void updateShuntCompensatorCreation(ShuntCompensatorCreationInfos shuntCompensatorCreationInfos, UUID modificationUuid) {
        assertShuntCompensatorCreationInfosNotEmpty(shuntCompensatorCreationInfos);

        Optional<ModificationEntity> shuntCompensatorModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!shuntCompensatorModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, "Shunt compensator creation not found");
        }

        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createShuntCompensatorEntity(shuntCompensatorCreationInfos);
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(shuntCompensatorModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private List<EquipmentModificationInfos> execCreateShuntCompensator(NetworkStoreListener listener,
                                                                        ShuntCompensatorCreationInfos shuntCompensatorCreationInfos,
                                                                        UUID reportUuid) {
        Network network = listener.getNetwork();
        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        String subReportId = "Shunt compensator creation " + shuntCompensatorCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the shunt compensator in the network
                VoltageLevel voltageLevel = getVoltageLevel(network, shuntCompensatorCreationInfos.getVoltageLevelId());
                createShuntCompensator(voltageLevel, shuntCompensatorCreationInfos, voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER);

                subReporter.report(Report.builder()
                    .withKey("shuntCompensatorCreated")
                    .withDefaultMessage("New shunt compensator with id=${id} created")
                    .withValue("id", shuntCompensatorCreationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the shunt compensator creation entity to the listener
            listener.storeShuntCompensatorCreation(shuntCompensatorCreationInfos);
        }, CREATE_SHUNT_COMPENSATOR_ERROR, reportUuid, reporter, subReporter).stream().map(EquipmentModificationInfos.class::cast)
            .collect(Collectors.toList());
    }

    public List<EquipmentModificationInfos> createShuntCompensator(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        assertShuntCompensatorCreationInfosNotEmpty(shuntCompensatorCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateShuntCompensator(listener, shuntCompensatorCreationInfos, reportUuid);
    }

    public void moveModifications(UUID groupUuid, UUID before, List<UUID> modificationsToMove) {
        networkModificationRepository.moveModifications(groupUuid, modificationsToMove, before);
    }

    public List<UUID> duplicateModifications(UUID targetGroupUuid, List<UUID> modificationsToDuplicate) {
        // This function cannot be @Transactional because we clone all modifications resetting their id to null,
        // which is not allowed by JPA if we still stay in the same Tx.
        List<ModificationEntity> newModificationList = new ArrayList<>();
        List<UUID> missingModificationList = new ArrayList<>();
        for (UUID modifyId : modificationsToDuplicate) {
            Optional<ModificationEntity> clone = this.modificationRepository.findById(modifyId);
            if (clone.isEmpty()) {
                missingModificationList.add(modifyId);  // data no more available
            } else {
                clone.get().setId(null);
                newModificationList.add(clone.get());
            }
        }
        networkModificationRepository.saveModifications(targetGroupUuid, newModificationList);
        return missingModificationList;
    }

    @Transactional
    public void updateModifyGenerator(UUID modificationUuid, GeneratorModificationInfos generatorModificationInfos) {
        assertEquipmentModificationInfosOk(generatorModificationInfos, MODIFICATION_NOT_FOUND);

        Optional<ModificationEntity> generatorModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (generatorModificationEntity.isEmpty()) {
            throw new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Generator modification not found");
        }
        GeneratorModificationEntity updatedEntity = this.networkModificationRepository.createGeneratorModificationEntity(generatorModificationInfos);
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(generatorModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);

    }

    private void assertLineSplitWithVoltageLevelInfosNotEmpty(LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        if (lineSplitWithVoltageLevelInfos == null) {
            throw new NetworkModificationException(LINE_SPLIT_ERROR,
                    "Missing required attributes to split a line");
        }
    }

    public List<ModificationInfos> splitLineWithVoltageLevel(UUID networkUuid, String variantId, GroupAndReportInfos groupAndReportInfos,
                                                              LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        assertLineSplitWithVoltageLevelInfosNotEmpty(lineSplitWithVoltageLevelInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupAndReportInfos.getGroupUuid(), networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return self.handleModification(lineSplitWithVoltageLevelInfos, listener, groupAndReportInfos).stream().map(ModificationInfos.class::cast).collect(Collectors.toList());
    }

    public void updateLineSplitWithVoltageLevel(UUID modificationUuid, LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        assertLineSplitWithVoltageLevelInfosNotEmpty(lineSplitWithVoltageLevelInfos);

        self.updateModification(lineSplitWithVoltageLevelInfos, modificationUuid);

    }

    private void assertLineAttachToVoltageLevelInfosNotEmpty(LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        if (lineAttachToVoltageLevelInfos == null) {
            throw new NetworkModificationException(LINE_ATTACH_ERROR,
                    "Missing required attributes to attach a line to a voltage level");
        }
    }

    private List<ModificationInfos> execLineAttachToVoltageLevel(NetworkStoreListener listener,
                                                                 LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos,
                                                                 UUID reportUuid) {
        Network network = listener.getNetwork();
        VoltageLevelCreationInfos mayNewVL = lineAttachToVoltageLevelInfos.getMayNewVoltageLevelInfos();
        LineCreationInfos attachmentLineInfos = lineAttachToVoltageLevelInfos.getAttachmentLine();
        if (attachmentLineInfos == null) {
            throw new NetworkModificationException(LINE_ATTACH_ERROR, "Missing required attachment line description");
        }

        ReporterModel reporter = new ReporterModel(NETWORK_MODIFICATION_REPORT_KEY, NETWORK_MODIFICATION_REPORT_NAME);
        Reporter subReporter = reporter.createSubReporter("lineAttachToVoltageLevel", "Line attach to voltage level");

        List<ModificationInfos> inspectable = doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Line line = network.getLine(lineAttachToVoltageLevelInfos.getLineToAttachToId());
                if (line == null) {
                    throw new NetworkModificationException(LINE_NOT_FOUND, lineAttachToVoltageLevelInfos.getLineToAttachToId());
                }

                String voltageLevelId;
                if (mayNewVL != null) {
                    ModificationUtils.getInstance().createVoltageLevelAction(mayNewVL, subReporter, network);
                    voltageLevelId = mayNewVL.getEquipmentId();
                } else {
                    voltageLevelId = lineAttachToVoltageLevelInfos.getExistingVoltageLevelId();
                }

                LineAdder lineAdder = network.newLine()
                        .setId(attachmentLineInfos.getEquipmentId())
                        .setName(attachmentLineInfos.getEquipmentName())
                        .setR(attachmentLineInfos.getSeriesResistance())
                        .setX(attachmentLineInfos.getSeriesReactance())
                        .setG1(attachmentLineInfos.getShuntConductance1() != null ? attachmentLineInfos.getShuntConductance1() : 0.0)
                        .setB1(attachmentLineInfos.getShuntSusceptance1() != null ? attachmentLineInfos.getShuntSusceptance1() : 0.0)
                        .setG2(attachmentLineInfos.getShuntConductance2() != null ? attachmentLineInfos.getShuntConductance2() : 0.0)
                        .setB2(attachmentLineInfos.getShuntSusceptance2() != null ? attachmentLineInfos.getShuntSusceptance2() : 0.0);

                AttachNewLineOnLine algo = new AttachNewLineOnLine(
                        lineAttachToVoltageLevelInfos.getPercent(),
                        voltageLevelId,
                        lineAttachToVoltageLevelInfos.getBbsOrBusId(),
                        lineAttachToVoltageLevelInfos.getAttachmentPointId(),
                        lineAttachToVoltageLevelInfos.getAttachmentPointName(),
                        true,
                        lineAttachToVoltageLevelInfos.getAttachmentPointId() + "_substation",
                        null,
                        lineAttachToVoltageLevelInfos.getNewLine1Id(),
                        lineAttachToVoltageLevelInfos.getNewLine1Name(),
                        lineAttachToVoltageLevelInfos.getNewLine2Id(),
                        lineAttachToVoltageLevelInfos.getNewLine2Name(),
                        line,
                        lineAdder
                );

                algo.apply(network);

                subReporter.report(Report.builder()
                        .withKey("lineAttach")
                        .withDefaultMessage("Line ${id} was attached")
                        .withValue("id", lineAttachToVoltageLevelInfos.getLineToAttachToId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }

            listener.storeLineAttachToVoltageLevelInfos(lineAttachToVoltageLevelInfos);
        }, LINE_ATTACH_ERROR, reportUuid, reporter, subReporter).stream().map(ModificationInfos.class::cast)
                .collect(Collectors.toList());

        if (!inspectable.isEmpty()) {
            inspectable.addAll(listener.getDeletions());
        }
        return inspectable;
    }

    public List<ModificationInfos> createLineAttachToVoltageLevel(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid,
                                                                  LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        assertLineAttachToVoltageLevelInfosNotEmpty(lineAttachToVoltageLevelInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execLineAttachToVoltageLevel(listener, lineAttachToVoltageLevelInfos, reportUuid);
    }

    public void updateLineAttachToVoltageLevel(UUID modificationUuid, LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        assertLineAttachToVoltageLevelInfosNotEmpty(lineAttachToVoltageLevelInfos);

        Optional<ModificationEntity> lineAttachToVoltageLevelEntity = this.modificationRepository.findById(modificationUuid);

        if (lineAttachToVoltageLevelEntity.isEmpty()) {
            throw new NetworkModificationException(LINE_ATTACH_NOT_FOUND, "Line attach not found");
        }

        LineAttachToVoltageLevelEntity casted = (LineAttachToVoltageLevelEntity) lineAttachToVoltageLevelEntity.get();
        VoltageLevelCreationEntity mayVoltageLevelCreation = casted.getMayVoltageLevelCreation();
        VoltageLevelCreationInfos mayNewVoltageLevelInfos = lineAttachToVoltageLevelInfos.getMayNewVoltageLevelInfos();
        LineCreationEntity lineCreation = casted.getLineCreation();
        LineCreationInfos lineInfos = lineAttachToVoltageLevelInfos.getAttachmentLine();

        LineAttachToVoltageLevelEntity updatedEntity = LineAttachToVoltageLevelEntity.toEntity(
                lineAttachToVoltageLevelInfos.getLineToAttachToId(),
                lineAttachToVoltageLevelInfos.getPercent(),
                lineAttachToVoltageLevelInfos.getAttachmentPointId(),
                lineAttachToVoltageLevelInfos.getAttachmentPointName(),
                mayNewVoltageLevelInfos,
                lineAttachToVoltageLevelInfos.getExistingVoltageLevelId(),
                lineAttachToVoltageLevelInfos.getBbsOrBusId(),
                lineInfos,
                lineAttachToVoltageLevelInfos.getNewLine1Id(),
                lineAttachToVoltageLevelInfos.getNewLine1Name(),
                lineAttachToVoltageLevelInfos.getNewLine2Id(),
                lineAttachToVoltageLevelInfos.getNewLine2Name()
        );
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(lineAttachToVoltageLevelEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);

        // NetworkStoreListener.makeVoltageLevelCreationEntity recreates on need, so get rid of previous
        if (mayVoltageLevelCreation != null) {
            this.modificationRepository.delete(mayVoltageLevelCreation);
        }
        if (lineCreation != null) {
            this.modificationRepository.delete(lineCreation);
        }
    }

    private Identifiable<?> getEquipmentByIdentifiableType(Network network, String type, String equipmentId) {
        if (type == null || equipmentId == null) {
            return null;
        }

        switch (IdentifiableType.valueOf(type)) {
            case HVDC_LINE:
                return network.getHvdcLine(equipmentId);
            case LINE:
                return network.getLine(equipmentId);
            case TWO_WINDINGS_TRANSFORMER:
                return network.getTwoWindingsTransformer(equipmentId);
            case THREE_WINDINGS_TRANSFORMER:
                return network.getThreeWindingsTransformer(equipmentId);
            case GENERATOR:
                return network.getGenerator(equipmentId);
            case LOAD:
                return network.getLoad(equipmentId);
            case BATTERY:
                return network.getBattery(equipmentId);
            case SHUNT_COMPENSATOR:
                return network.getShuntCompensator(equipmentId);
            case STATIC_VAR_COMPENSATOR:
                return network.getStaticVarCompensator(equipmentId);
            case DANGLING_LINE:
                return network.getDanglingLine(equipmentId);
            case HVDC_CONVERTER_STATION:
                return network.getHvdcConverterStation(equipmentId);
            case SUBSTATION:
                return network.getSubstation(equipmentId);
            case VOLTAGE_LEVEL:
                return network.getVoltageLevel(equipmentId);
            case BUSBAR_SECTION:
                return network.getBusbarSection(equipmentId);
            default:
                return null;
        }
    }

    private Terminal getTerminalFromIdentifiable(Network network,
                                                 String equipmentId,
                                                 String type,
                                                 String voltageLevelId) {
        if (network != null && equipmentId != null && type != null && voltageLevelId != null) {
            Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, type, equipmentId);

            if (identifiable == null) {
                throw new NetworkModificationException(EQUIPMENT_NOT_FOUND);
            }

            if (identifiable instanceof Injection<?>) {
                return ((Injection<?>) identifiable).getTerminal();
            } else if (identifiable instanceof Branch<?>) {
                return ((Branch<?>) identifiable).getTerminal(voltageLevelId);
            }
        }

        return null;
    }
}
