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
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import com.powsybl.sld.iidm.extensions.BranchStatusAdder;
import com.powsybl.iidm.network.Branch.Side;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.EquipmentType;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.DefaultUriBuilderFactory;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
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

    public Flux<EquipmenModificationInfos> applyGroovyScript(UUID networkUuid, UUID groupUuid, String groovyScript) {
        return assertGroovyScriptNotEmpty(groovyScript).thenMany(
            getNetwork(networkUuid).flatMapIterable(network -> doAction(network, networkUuid, groupUuid, () -> {
                var conf = new CompilerConfiguration();
                var binding = new Binding();
                binding.setProperty("network", network);
                var shell = new GroovyShell(binding, conf);
                shell.evaluate(groovyScript);
            }, GROOVY_SCRIPT_ERROR))
        );
    }

    public Flux<EquipmenModificationInfos> changeSwitchState(UUID networkUuid, UUID groupUuid, String switchId, boolean open) {
        return getNetwork(networkUuid)
            .filter(network -> network.getSwitch(switchId) != null)
            .switchIfEmpty(Mono.error(new NetworkModificationException(SWITCH_NOT_FOUND, switchId)))
            .filter(network -> network.getSwitch(switchId).isOpen() != open)
            .flatMapIterable(network -> doAction(network, networkUuid, groupUuid, () -> network.getSwitch(switchId).setOpen(open)));
    }

    public Flux<UUID> getModificationGroups() {
        return Flux.fromStream(() -> modificationRepository.getModificationGroupsUuids().stream());
    }

    public Flux<ModificationInfos> getModifications(UUID groupUuid) {
        return Flux.fromStream(() -> modificationRepository.getModifications(groupUuid).stream());
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();
        return terminal1Disconnected && terminal2Disconnected;
    }

    public Flux<EquipmenModificationInfos> changeLineStatus(UUID networkUuid, UUID groupUuid, String lineId, String lineStatus) {
        Flux<EquipmenModificationInfos> modifications;
        switch (lineStatus) {
            case "lockout":
                modifications = lockoutLine(networkUuid, groupUuid, lineId);
                break;
            case "trip":
                modifications = tripLine(networkUuid, groupUuid, lineId);
                break;
            case "switchOn":
                modifications = switchOnLine(networkUuid, groupUuid, lineId);
                break;
            case "energiseEndOne":
                modifications = energiseLineEnd(networkUuid, groupUuid, lineId, Branch.Side.ONE);
                break;
            case "energiseEndTwo":
                modifications = energiseLineEnd(networkUuid, groupUuid, lineId, Branch.Side.TWO);
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + lineStatus);
        }
        return modifications;
    }

    public Flux<EquipmenModificationInfos> lockoutLine(UUID networkUuid, UUID groupUuid, String lineId) {
        return getNetwork(networkUuid)
            .filter(network -> network.getLine(lineId) != null)
            .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
            .flatMapIterable(network -> doAction(network, networkUuid, groupUuid, () -> {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends");
                    }
                }
            ));
    }

    public Flux<EquipmenModificationInfos> tripLine(UUID networkUuid, UUID groupUuid, String lineId) {
        return getNetwork(networkUuid)
            .filter(network -> network.getLine(lineId) != null)
            .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
            .flatMapIterable(network -> doAction(network, networkUuid, groupUuid, () -> {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends");
                    }
                }
            ));
    }

    public Flux<EquipmenModificationInfos> energiseLineEnd(UUID networkUuid, UUID groupUuid, String lineId, Branch.Side side) {
        return getNetwork(networkUuid)
            .filter(network -> network.getLine(lineId) != null)
            .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
            .flatMapIterable(network -> doAction(network, networkUuid, groupUuid, () -> {
                    Terminal terminalToConnect = network.getLine(lineId).getTerminal(side);
                    boolean isTerminalToConnectConnected = terminalToConnect.isConnected() || terminalToConnect.connect();
                    Terminal terminalToDisconnect = network.getLine(lineId).getTerminal(side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE);
                    boolean isTerminalToDisconnectDisconnected = !terminalToDisconnect.isConnected() || terminalToDisconnect.disconnect();
                    if (isTerminalToConnectConnected && isTerminalToDisconnectDisconnected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to energise line end");
                    }
                }
            ));
    }

    public Flux<EquipmenModificationInfos> switchOnLine(UUID networkUuid, UUID groupUuid, String lineId) {
        return getNetwork(networkUuid)
            .filter(network -> network.getLine(lineId) != null)
            .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
            .flatMapIterable(network -> doAction(network, networkUuid, groupUuid, () -> {
                    Terminal terminal1 = network.getLine(lineId).getTerminal1();
                    boolean terminal1Connected = terminal1.isConnected() || terminal1.connect();
                    Terminal terminal2 = network.getLine(lineId).getTerminal2();
                    boolean terminal2Connected = terminal2.isConnected() || terminal2.connect();
                    if (terminal1Connected && terminal2Connected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to connect both line ends");
                    }
                }
            ));
    }

    public Mono<Void> deleteModificationGroup(UUID groupUuid) {
        return Mono.fromRunnable(() -> modificationRepository.deleteModificationGroup(groupUuid));
    }

    private List<EquipmenModificationInfos> doAction(Network network, UUID networkUuid, UUID groupUuid, Runnable modification) {
        return doAction(network, networkUuid, groupUuid, modification, MODIFICATION_ERROR);
    }

    private List<EquipmenModificationInfos> doAction(Network network, UUID networkUuid, UUID groupUuid, Runnable modification, NetworkModificationException.Type typeIfError) {
        NetworkStoreListener listener = NetworkStoreListener.create(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService);
        ReporterModel reporter = new ReporterModel("NetworkModification", "Network modification");
        return doAction(listener, modification, typeIfError, networkUuid, reporter, reporter);
    }

    private List<EquipmenModificationInfos> doAction(NetworkStoreListener listener, Runnable action,
                                                     NetworkModificationException.Type typeIfError,
                                                     UUID networkUuid, ReporterModel reporter, Reporter subReporter) {
        try {
            action.run();
            saveModifications(listener);
            return listener.getModifications();
        } catch (Exception e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(typeIfError, e);
            subReporter.report(Report.builder()
                .withKey(typeIfError.name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(new TypedValue("NETWORK_MODIFICATION_ERROR", TypedValue.ERROR_LOGLEVEL))
                .build());
            throw exc;
        } finally {
            // send report
            sendReport(networkUuid, reporter);
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

    private Mono<Network> getNetwork(UUID networkUuid) {
        return Mono.fromCallable(() -> {
            try {
                return networkStoreService.getNetwork(networkUuid);
            } catch (PowsyblException e) {
                throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
            }
        }).subscribeOn(Schedulers.boundedElastic());
    }

    private Mono<Void> assertGroovyScriptNotEmpty(String groovyScript) {
        return StringUtils.isBlank(groovyScript) ? Mono.error(new NetworkModificationException(GROOVY_SCRIPT_EMPTY)) : Mono.empty();
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

    public Flux<EquipmenModificationInfos> createLoad(UUID networkUuid, UUID groupUuid, LoadCreationInfos loadCreationInfos) {
        return assertLoadCreationInfosNotEmpty(loadCreationInfos).thenMany(
                getNetwork(networkUuid).flatMapIterable(network -> {
                    NetworkStoreListener listener = NetworkStoreListener.create(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService);
                    ReporterModel reporter = new ReporterModel("NetworkModification", "Network modification");
                    Reporter subReporter = reporter.createSubReporter("LoadCreation", "Load creation");

                    return doAction(listener, () -> {
                        // create the load in the network
                        VoltageLevel voltageLevel = getVoltageLevel(network, loadCreationInfos.getVoltageLevelId());
                        Load load;
                        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                            load = createLoadInNodeBreaker(voltageLevel, loadCreationInfos);
                        } else {
                            load = createLoadInBusBreaker(voltageLevel, loadCreationInfos);
                        }

                        // store the substations ids in the listener
                        listener.setSubstationsIds(NetworkStoreListener.getSubstationIds(load));

                        subReporter.report(Report.builder()
                            .withKey("loadCreated")
                            .withDefaultMessage("New load with id=${id} created")
                            .withValue("id", loadCreationInfos.getEquipmentId())
                            .withSeverity(new TypedValue("LOAD_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                            .build());

                        // add the load creation entity to the listener
                        listener.storeLoadCreation(loadCreationInfos);
                    }, CREATE_LOAD_ERROR, networkUuid, reporter, subReporter);
                }));
    }

    private Mono<Void> assertLoadCreationInfosNotEmpty(LoadCreationInfos loadCreationInfos) {
        return loadCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_LOAD_ERROR, "Missing required attributes to create the load")) : Mono.empty();
    }

    public Flux<EquipmentDeletionInfos> deleteEquipment(UUID networkUuid, UUID groupUuid, String equipmentType, String equipmentId) {
        return getNetwork(networkUuid).flatMapIterable(network -> {
            NetworkStoreListener listener = NetworkStoreListener.create(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService);
            ReporterModel reporter = new ReporterModel("NetworkModification", "Network modification");
            Reporter subReporter = reporter.createSubReporter("EquipmentDeletion", "Equipment deletion");

            return doAction(listener, () -> {
                Identifiable identifiable = null;
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
                    case LCC_CONVERTER_STATION:
                        identifiable = network.getLccConverterStation(equipmentId);
                        break;
                    case VSC_CONVERTER_STATION:
                        identifiable = network.getVscConverterStation(equipmentId);
                        break;
                    default:
                        break;
                }
                if (identifiable == null) {
                    throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + equipmentId + " not found or of bad type");
                }

                // store the substations ids in the listener
                listener.setSubstationsIds(NetworkStoreListener.getSubstationIds(identifiable));

                if (identifiable instanceof Connectable) {
                    ((Connectable) identifiable).remove();
                } else if (identifiable instanceof HvdcLine) {
                    ((HvdcLine) identifiable).remove();
                }

                // Done here, and not in the network listener onRemoval method
                // because onRemoval must be refactored in powsybl core
                listener.deleteEquipmentInfos(equipmentId);

                subReporter.report(Report.builder()
                    .withKey("equipmentDeleted")
                    .withDefaultMessage("equipment of type=${type} and id=${id} deleted")
                    .withValue("type", equipmentType)
                    .withValue("id", equipmentId)
                    .withSeverity(new TypedValue("EQUIPMENT_DELETION_INFO", TypedValue.INFO_LOGLEVEL))
                    .build());

                // add the equipment deletion entity to the listener
                listener.storeEquipmentDeletion(equipmentId, equipmentType);
            }, DELETE_EQUIPMENT_ERROR, networkUuid, reporter, subReporter).stream().map(EquipmentDeletionInfos.class::cast)
                .collect(Collectors.toList());
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

    public Flux<EquipmenModificationInfos> createGenerator(UUID networkUuid, UUID groupUuid, GeneratorCreationInfos generatorCreationInfos) {
        return assertGeneratorCreationInfosNotEmpty(generatorCreationInfos).thenMany(
            getNetwork(networkUuid).flatMapIterable(network -> {
                NetworkStoreListener listener = NetworkStoreListener.create(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService);
                ReporterModel reporter = new ReporterModel("NetworkModification", "Network modification");
                Reporter subReporter = reporter.createSubReporter("GeneratorCreation", "Generator creation");

                return doAction(listener, () -> {
                    // create the generator in the network
                    VoltageLevel voltageLevel = getVoltageLevel(network, generatorCreationInfos.getVoltageLevelId());
                    Generator generator;
                    if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                        generator = createGeneratorInNodeBreaker(voltageLevel, generatorCreationInfos);
                    } else {
                        generator = createGeneratorInBusBreaker(voltageLevel, generatorCreationInfos);
                    }

                    // store the substations ids in the listener
                    listener.setSubstationsIds(NetworkStoreListener.getSubstationIds(generator));

                    subReporter.report(Report.builder()
                        .withKey("generatorCreated")
                        .withDefaultMessage("New generator with id=${id} created")
                        .withValue("id", generatorCreationInfos.getEquipmentId())
                        .withSeverity(new TypedValue("GENERATOR_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                        .build());

                    // add the generator creation entity to the listener
                    listener.storeGeneratorCreation(generatorCreationInfos);
                }, CREATE_GENERATOR_ERROR, networkUuid, reporter, subReporter);
            }));
    }

    private Mono<Void> assertGeneratorCreationInfosNotEmpty(GeneratorCreationInfos generatorCreationInfos) {
        return generatorCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Missing required attributes to create the generator")) : Mono.empty();
    }

    private void setLineAdderNodeOrBus(LineAdder lineAdder, VoltageLevel voltageLevel, LineCreationInfos lineCreationInfos, Side side) {
        String currentBusBarSectionId = (side == Side.ONE) ? lineCreationInfos.getBusOrBusbarSectionId1() : lineCreationInfos.getBusOrBusbarSectionId2();

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
                lineCreationInfos.getEquipmentId(),
                lineCreationInfos.getEquipmentName(),
                sideSuffix);

            // complete the lineAdder
            if (side == Side.ONE) {
                lineAdder.setNode1(nodeNum);
            } else {
                lineAdder.setNode2(nodeNum);
            }
        } else { // BUS BREAKER
            // busId is a bus id
            Bus bus = getBusBreakerBus(voltageLevel, currentBusBarSectionId);

            // complete the lineAdder
            if (side == Side.ONE) {
                lineAdder.setBus1(bus.getId()).setConnectableBus1(bus.getId());
            } else {
                lineAdder.setBus2(bus.getId()).setConnectableBus2(bus.getId());
            }
        }

    }

    private void createLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos) {

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
        setLineAdderNodeOrBus(lineAdder, voltageLevel1, lineCreationInfos, Side.ONE);
        setLineAdderNodeOrBus(lineAdder, voltageLevel2, lineCreationInfos, Side.TWO);

        lineAdder.add();
    }

    public Flux<EquipmenModificationInfos> createLine(UUID networkUuid, UUID groupUuid, LineCreationInfos lineCreationInfos) {
        return assertLineCreationInfosNotEmpty(lineCreationInfos).thenMany(
                getNetwork(networkUuid).flatMapIterable(network -> {
                    NetworkStoreListener listener = NetworkStoreListener.create(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService);
                    ReporterModel reporter = new ReporterModel("NetworkModification", "Network modification");
                    Reporter subReporter = reporter.createSubReporter("LineCreation", "Line creation");

                    return doAction(listener, () -> {
                        // create the line in the network
                        VoltageLevel voltageLevel1 = getVoltageLevel(network, lineCreationInfos.getVoltageLevelId1());
                        VoltageLevel voltageLevel2 = getVoltageLevel(network, lineCreationInfos.getVoltageLevelId2());

                        createLine(network, voltageLevel1, voltageLevel2, lineCreationInfos);

                        subReporter.report(Report.builder()
                            .withKey("lineCreated")
                            .withDefaultMessage("New line with id=${id} created")
                            .withValue("id", lineCreationInfos.getEquipmentId())
                            .withSeverity(new TypedValue("LINE_CREATION_INFO", TypedValue.INFO_LOGLEVEL))
                            .build());

                        // add the line creation entity to the listener
                        listener.storeLineCreation(lineCreationInfos);
                    }, CREATE_LINE_ERROR, networkUuid, reporter, subReporter);
                }));
    }

    private Mono<Void> assertLineCreationInfosNotEmpty(LineCreationInfos lineCreationInfos) {
        return lineCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_LINE_ERROR, "Missing required attributes to create the line")) : Mono.empty();
    }
}
