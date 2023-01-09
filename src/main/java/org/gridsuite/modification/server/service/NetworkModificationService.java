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
import com.powsybl.iidm.modification.topology.*;
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.Branch.Side;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import com.powsybl.network.store.iidm.impl.extensions.GeneratorStartupAdderImpl;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos.ActionType;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.EquipmentCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.TwoWindingsTransformerCreationEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.modifications.ModificationApplicator;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
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
    private final ModificationRepository modificationRepository;

    private final EquipmentInfosService equipmentInfosService;

    private final NotificationService notificationService;

    private RestTemplate reportServerRest = new RestTemplate();

    private String reportServerBaseUri;

    private final ObjectMapper objectMapper;

    private static final String REPORT_API_VERSION = "v1";
    private static final String DELIMITER = "/";

    private static final String NETWORK_MODIFICATION_TYPE_REPORT = "NetworkModification";
    private static final String LINE_ID_PARAMETER = "lineId";
    private static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    private static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";

    public NetworkModificationService(@Value("${gridsuite.services.report-server.base-uri:http://report-server}") String reportServerURI,
                                      NetworkStoreService networkStoreService, NetworkModificationRepository networkModificationRepository,
                                      EquipmentInfosService equipmentInfosService,
                                      ModificationRepository modificationRepository, NotificationService notificationService,
                                      ModificationApplicator modificationApplicator, ObjectMapper objectMapper) {
        this.networkStoreService = networkStoreService;
        this.networkModificationRepository = networkModificationRepository;
        this.equipmentInfosService = equipmentInfosService;
        this.modificationRepository = modificationRepository;
        this.notificationService = notificationService;
        this.modificationApplicator = modificationApplicator;

        this.reportServerBaseUri = reportServerURI;
        this.objectMapper = objectMapper;
        this.objectMapper.registerModule(new ReporterModelJsonModule());
        this.objectMapper.setInjectableValues(new InjectableValues.Std().addValue(ReporterModelDeserializer.DICTIONARY_VALUE_ID, null));
    }

    public void setReportServerBaseUri(String reportServerBaseUri) {
        this.reportServerBaseUri = reportServerBaseUri;
    }

    private String getReportServerURI() {
        return this.reportServerBaseUri + DELIMITER + REPORT_API_VERSION + DELIMITER + "reports" + DELIMITER;
    }

    private List<ModificationInfos> execCreateGroovyScript(NetworkStoreListener listener,
                                                           String groovyScript,
                                                           UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.GROOVY_SCRIPT.name(), "Apply groovy script");

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

    public List<ModificationInfos> createGroovyScript(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, GroovyScriptModificationInfos groovyScript) {
        assertGroovyScriptNotEmpty(groovyScript);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateGroovyScript(listener, groovyScript.getScript(), reportUuid, reporterId);
    }

    public List<UUID> getModificationGroups() {
        return networkModificationRepository.getModificationGroupsUuids();
    }

    @Transactional(readOnly = true)
    // Need a transaction for collections lazy loading
    public List<ModificationInfos> getNetworkModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        return networkModificationRepository.getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound);
    }

    @Transactional(readOnly = true)
    public ModificationInfos getNetworkModification(UUID networkModificationUuid) {
        return networkModificationRepository.getModificationInfo(networkModificationUuid);
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();
        return terminal1Disconnected && terminal2Disconnected;
    }

    public List<ModificationInfos> createLineStatusModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, BranchStatusModificationInfos branchStatusModificationInfos) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateBranchStatusModification(listener, branchStatusModificationInfos, reportUuid, reporterId);
    }

    private List<ModificationInfos> execCreateBranchStatusModification(NetworkStoreListener listener, BranchStatusModificationInfos branchStatusModificationInfos, UUID reportUuid, String reporterId) {
        String lineId = branchStatusModificationInfos.getEquipmentId();
        BranchStatusModificationInfos.ActionType action = branchStatusModificationInfos.getAction();
        if (action == null) {
            throw new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY);
        }
        switch (action) {
            case LOCKOUT:
                return execCreateLockoutLine(listener, lineId, reportUuid, reporterId);
            case TRIP:
                return execCreateTripLine(listener, lineId, reportUuid, reporterId);
            case SWITCH_ON:
                return execCreateSwitchOnLine(listener, lineId, reportUuid, reporterId);
            case ENERGISE_END_ONE:
                return execCreateEnergiseLineEnd(listener, lineId, Branch.Side.ONE, reportUuid, reporterId);
            case ENERGISE_END_TWO:
                return execCreateEnergiseLineEnd(listener, lineId, Side.TWO, reportUuid, reporterId);
            default:
                throw NetworkModificationException.createBranchActionTypeUnsupported(action);
        }
    }

    private List<ModificationInfos> execCreateLockoutLine(NetworkStoreListener listener, String lineId, UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(
                ModificationType.BRANCH_STATUS.name() + "_" + ActionType.LOCKOUT.name(),
                "Lockout line ${lineId}", LINE_ID_PARAMETER, lineId);

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

    private List<ModificationInfos> execCreateTripLine(NetworkStoreListener listener, String lineId, UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter
                .createSubReporter(
                        ModificationType.BRANCH_STATUS.name() + "_" + ActionType.TRIP.name(),
                        "Trip line ${lineId}", LINE_ID_PARAMETER, lineId);

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

    private List<ModificationInfos> execCreateEnergiseLineEnd(NetworkStoreListener listener,
                                                              String lineId,
                                                              Side side,
                                                              UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        String branchSideKey = side == Branch.Side.ONE ? ActionType.ENERGISE_END_TWO.name() : ActionType.ENERGISE_END_ONE.name();
        Reporter subReporter = reporter
                .createSubReporter(
                        ModificationType.BRANCH_STATUS.name() + "_" + branchSideKey,
                        "Energise line ${lineId}", LINE_ID_PARAMETER, lineId);

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

    private List<ModificationInfos> execCreateSwitchOnLine(NetworkStoreListener listener, String lineId, UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(
                ModificationType.BRANCH_STATUS.name() + "_" + ActionType.SWITCH_ON.name(),
                "Switch on line ${lineId}", LINE_ID_PARAMETER, lineId);

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

    private void assertGroovyScriptNotEmpty(GroovyScriptModificationInfos groovyScript) {
        if (StringUtils.isBlank(groovyScript.getScript())) {
            throw new NetworkModificationException(GROOVY_SCRIPT_EMPTY);
        }
    }

    private VoltageLevel getVoltageLevel(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId);
        }
        return voltageLevel;
    }

    private Generator getGenerator(Network network, String generatorId) {
        Generator generator = network.getGenerator(generatorId);
        if (generator == null) {
            throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + generatorId + " does not exist in network");
        }
        return generator;
    }

    public void updateGeneratorCreation(UUID modificationUuid, GeneratorCreationInfos generatorCreationInfos) {
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
                generatorCreationInfos.getPlannedActivePowerSetPoint(),
                generatorCreationInfos.getStartupCost(),
                generatorCreationInfos.getMarginalCost(),
                generatorCreationInfos.getPlannedOutageRate(),
                generatorCreationInfos.getForcedOutageRate(),
                generatorCreationInfos.getMinimumReactivePower(),
                generatorCreationInfos.getMaximumReactivePower(),
                generatorCreationInfos.getParticipate(),
                generatorCreationInfos.getDroop(),
                generatorCreationInfos.getTransientReactance(),
                generatorCreationInfos.getStepUpTransformerReactance(),
                generatorCreationInfos.getRegulatingTerminalId(),
                generatorCreationInfos.getRegulatingTerminalType(),
                generatorCreationInfos.getRegulatingTerminalVlId(),
                generatorCreationInfos.getQPercent(),
                generatorCreationInfos.getReactiveCapabilityCurve(),
                toEmbeddablePoints(generatorCreationInfos.getReactiveCapabilityCurvePoints()),
                generatorCreationInfos.getConnectionName(),
                generatorCreationInfos.getConnectionDirection(),
                generatorCreationInfos.getConnectionPosition());

        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(generatorModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    // Generic form
    private List<ModificationInfos> handleModification(ModificationInfos modificationInfos, NetworkStoreListener listener, UUID groupUuid, UUID reportUuid, String reporterId) {
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        List<ModificationInfos> networkModifications = List.of();
        try {
            if (listener.isApplyModifications()) {
                networkModifications = modificationApplicator.apply(modificationInfos, reporter, listener);
            }
            if (!listener.isBuild()) {
                saveModifications(listener, groupUuid, modificationInfos.toEntity());
            }
            return networkModifications;
        } finally {
            if (listener.isApplyModifications()) {
                sendReport(reportUuid, reporter);
            }
        }
    }

    // temporary wildcard code smell (method to be deleted)
    public List<? extends ModificationInfos> createNetworkModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, ModificationInfos modificationInfos) {

        switch (modificationInfos.getType()) {
            case GROOVY_SCRIPT:
                return createGroovyScript(networkUuid, variantId, groupUuid, reportUuid, reporterId, (GroovyScriptModificationInfos) modificationInfos);
            case LOAD_MODIFICATION:
                return createLoadModification(networkUuid, variantId, groupUuid, reportUuid, reporterId, (LoadModificationInfos) modificationInfos);
            case GENERATOR_CREATION:
                return createGeneratorCreation(networkUuid, variantId, groupUuid, reportUuid, reporterId, (GeneratorCreationInfos) modificationInfos);
            case GENERATOR_MODIFICATION:
                return createGeneratorModification(networkUuid, variantId, groupUuid, reportUuid, reporterId, (GeneratorModificationInfos) modificationInfos);
            case SUBSTATION_CREATION:
                return createSubstationCreation(networkUuid, variantId, groupUuid, reportUuid, reporterId, (SubstationCreationInfos) modificationInfos);
            case TWO_WINDINGS_TRANSFORMER_CREATION:
                return createTwoWindingsTransformerCreation(networkUuid, variantId, groupUuid, reportUuid, reporterId, (TwoWindingsTransformerCreationInfos) modificationInfos);
            case EQUIPMENT_DELETION:
                return createEquipmentDeletion(networkUuid, variantId, groupUuid, reportUuid, reporterId, (EquipmentDeletionInfos) modificationInfos);
            case BRANCH_STATUS:
                return createLineStatusModification(networkUuid, variantId, groupUuid, reportUuid, reporterId, (BranchStatusModificationInfos) modificationInfos);
            default:
                throw new NetworkModificationException(TYPE_MISMATCH);
        }
    }

    public void updateNetworkModification(UUID modificationUuid, ModificationInfos modificationInfos) {

        switch (modificationInfos.getType()) {
            case LOAD_MODIFICATION:
                updateLoadModification(modificationUuid, (LoadModificationInfos) modificationInfos);
                break;
            case GENERATOR_CREATION:
                updateGeneratorCreation(modificationUuid, (GeneratorCreationInfos) modificationInfos);
                break;
            case GENERATOR_MODIFICATION:
                updateGeneratorModification(modificationUuid, (GeneratorModificationInfos) modificationInfos);
                break;
            case SUBSTATION_CREATION:
                updateSubstationCreation(modificationUuid, (SubstationCreationInfos) modificationInfos);
                break;
            case TWO_WINDINGS_TRANSFORMER_CREATION:
                updateTwoWindingsTransformerCreation(modificationUuid, (TwoWindingsTransformerCreationInfos) modificationInfos);
                break;
            case EQUIPMENT_DELETION:
                updateEquipmentDeletion(modificationUuid, (EquipmentDeletionInfos) modificationInfos);
                break;
            default:
                throw new NetworkModificationException(TYPE_MISMATCH);
        }
    }

    private void saveModifications(NetworkStoreListener listener, UUID groupUuid, ModificationEntity modificationEntity) {
        networkModificationRepository.saveModifications(groupUuid, List.of(modificationEntity));
        if (listener.isApplyModifications()) {
            networkStoreService.flush(listener.getNetwork());
        }
    }

    // Generic form
    @Transactional
    public void updateModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
        ModificationEntity modificationEntity = this.modificationRepository.findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)));

        modificationEntity.update(modificationInfos);
    }

    @Transactional
    public List<ModificationInfos> createModification(@NonNull UUID networkUuid, String variantId, @NonNull UUID groupUuid, @NonNull UUID reportUuid, @NonNull String reporterId, @NonNull ModificationInfos modificationInfos) {
        modificationInfos.check();
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return handleModification(modificationInfos, listener, groupUuid, reportUuid, reporterId);
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

    public void updateLoadModification(UUID modificationUuid, LoadModificationInfos loadModificationInfos) {
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

    private List<ModificationInfos> execCreateLoadModification(NetworkStoreListener listener,
                                                   LoadModificationInfos loadModificationInfos,
                                                   UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.LOAD_MODIFICATION.name(), "Load modification ${loadId}", "loadId", loadModificationInfos.getEquipmentId());

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
        }, MODIFY_LOAD_ERROR, reportUuid, reporter, subReporter);
    }

    private static <T> void applyElementaryModifications(Consumer<T> setter, Supplier<T> getter,
                                                         AttributeModification<T> modification,
                                                         Reporter subReporter, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            addModificationReport(oldValue, newValue, subReporter, fieldName);
        }
    }

    private static <T> void addModificationReport(T oldValue, T newValue,
            Reporter subReporter, String fieldName) {
        String oldValueString = oldValue == null ? "NaN" : oldValue.toString();
        String newValueString = newValue == null ? "NaN" : newValue.toString();
        subReporter.report(Report.builder()
                .withKey("Modification" + fieldName)
                .withDefaultMessage("    ${fieldName} : ${oldValue} -> ${newValue}")
                .withValue("fieldName", fieldName)
                .withValue("oldValue", oldValueString)
                .withValue("newValue", newValueString)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void modifyGeneratorShortCircuitAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        // Either transient reactance or step-up transformer reactance are modified or
        // both
        if (modificationInfos.getTransientReactance() != null
                && modificationInfos.getStepUpTransformerReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(modificationInfos.getTransientReactance().getValue())
                    .withStepUpTransformerX(modificationInfos.getStepUpTransformerReactance().getValue())
                    .add();
            addModificationReport(generatorShortCircuit != null ? generatorShortCircuit.getDirectTransX() : Double.NaN,
                    modificationInfos.getTransientReactance().getValue(), subReporter,
                    "Transient reactance");
            addModificationReport(generatorShortCircuit != null ? generatorShortCircuit.getStepUpTransformerX() : Double.NaN,
                    modificationInfos.getStepUpTransformerReactance().getValue(), subReporter,
                    "Transformer reactance");

        } else if (modificationInfos.getTransientReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(modificationInfos.getTransientReactance().getValue())
                    .add();
            addModificationReport(generatorShortCircuit != null ? generatorShortCircuit.getDirectTransX() : Double.NaN,
                    modificationInfos.getTransientReactance().getValue(), subReporter,
                    "Transient reactance");
        } else if (modificationInfos.getStepUpTransformerReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withStepUpTransformerX(modificationInfos.getStepUpTransformerReactance().getValue())
                    .add();
            addModificationReport(generatorShortCircuit != null ? generatorShortCircuit.getStepUpTransformerX() : Double.NaN,
                    modificationInfos.getStepUpTransformerReactance().getValue(), subReporter,
                    "Transformer reactance");
        }
    }

    private void modifyGeneratorMinMaxReactiveLimits(GeneratorModificationInfos modificationInfos, Generator generator,
            Reporter subReporter) {
        //we get previous min max values if they exist
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = generator.getReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = generator.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }

        // (if the min and max reactive limits are null and there is no previous min max limits set we set them to Double max and
        // Double min values)
        // The user can change the value of MinimumReactivePower, MaximumReactivePower or both
        if (modificationInfos.getMinimumReactivePower() != null
                && modificationInfos.getMaximumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(modificationInfos.getMinimumReactivePower().getValue())
                    .setMaxQ(modificationInfos.getMaximumReactivePower().getValue())
                    .add();
            addModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    modificationInfos.getMinimumReactivePower().getValue(), subReporter,
                    MIN_REACTIVE_POWER_FIELDNAME);
            addModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    modificationInfos.getMaximumReactivePower().getValue(), subReporter,
                    MAX_REACTIVE_POWER_FIELDNAME);
        } else if (modificationInfos.getMinimumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(modificationInfos.getMinimumReactivePower().getValue())
                    .setMaxQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.MAX_VALUE)
                    .add();
            addModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    modificationInfos.getMinimumReactivePower().getValue(), subReporter,
                    MIN_REACTIVE_POWER_FIELDNAME);
        } else if (modificationInfos.getMaximumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : -Double.MAX_VALUE)
                    .setMaxQ(modificationInfos.getMaximumReactivePower().getValue())
                    .add();
            addModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    modificationInfos.getMaximumReactivePower().getValue(), subReporter,
                    MAX_REACTIVE_POWER_FIELDNAME);
        } else if (minMaxReactiveLimits == null) {
            generator.newMinMaxReactiveLimits().setMinQ(-Double.MAX_VALUE)
                    .setMaxQ(Double.MAX_VALUE)
                    .add();
            addModificationReport(Double.NaN,
                    -Double.MAX_VALUE, subReporter,
                    MIN_REACTIVE_POWER_FIELDNAME);
            addModificationReport(Double.NaN,
                    Double.MAX_VALUE, subReporter,
                    MAX_REACTIVE_POWER_FIELDNAME);
        }
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> points = modificationInfos.getReactiveCapabilityCurvePoints();
        IntStream.range(0, points.size())
                .forEach(i -> {
                    ReactiveCapabilityCurveModificationInfos point = points.get(i);
                    adder.beginPoint()
                            .setMaxQ(point.getQmaxP() != null ? point.getQmaxP() : point.getOldQmaxP())
                            .setMinQ(point.getQminP() != null ? point.getQminP() : point.getOldQminP())
                            .setP(point.getP() != null ? point.getP() : point.getOldP())
                            .endPoint();
                    if (point.getP() != null) {
                        addModificationReport(point.getOldP(),
                                point.getP(), subReporter,
                                "P" + i);
                    }
                    if (point.getQminP() != null) {
                        addModificationReport(point.getOldQminP(),
                                point.getQminP(), subReporter,
                                "QminP" + i);
                    }
                    if (point.getQmaxP() != null) {
                        addModificationReport(point.getOldQmaxP(),
                                point.getQmaxP(), subReporter,
                                "QmaxP" + i);
                    }
                });
        adder.add();
    }

    private void modifyGeneratorReactiveLimitsAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                && modificationInfos.getReactiveCapabilityCurvePoints() != null
                && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
            modifyGeneratorReactiveCapabilityCurvePoints(modificationInfos, generator, subReporter);
        } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
            modifyGeneratorMinMaxReactiveLimits(modificationInfos, generator, subReporter);
        }
    }

    private void modifyGeneratorActivePowerControlAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        Float oldDroop = activePowerControl != null ? activePowerControl.getDroop() : Float.NaN;
        Boolean participate = null;
        // if participate is null and droop was modified, we consider that participate
        // is true
        if (modificationInfos.getParticipate() != null) {
            participate = modificationInfos.getParticipate().getValue();
            addModificationReport(activePowerControl != null ? activePowerControl.isParticipate() : null,
                    participate, subReporter,
                    "Active power regulation");
        } else if (modificationInfos.getDroop() != null) {
            participate = true;
        }
        // if no modification were done to ActivePowerControl, we don't apply
        // modifications
        if (participate != null) {
            if (Boolean.TRUE.equals(participate)) {
                generator.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(participate).withDroop(modificationInfos.getDroop().getValue())
                        .add();
                addModificationReport(oldDroop,
                        modificationInfos.getDroop().getValue(), subReporter,
                        "Droop");
            } else {
                generator.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(participate).add();
            }
        }

    }

    private void modifyGeneratorStartUpAttributes(GeneratorModificationInfos modificationInfos, Generator generator,
            Reporter subReporter) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        Double oldMarginalCost = generatorStartup != null ? generatorStartup.getMarginalCost() : Double.NaN;
        if (modificationInfos.getMarginalCost() != null) {
            generator.newExtension(GeneratorStartupAdder.class)
                    .withMarginalCost(modificationInfos.getMarginalCost().getValue()).add();

            addModificationReport(oldMarginalCost,
                    modificationInfos.getMarginalCost().getValue(), subReporter,
                    "Cost of start");

        }
    }

    private void modifyGeneratorRegulatingTerminal(GeneratorModificationInfos modificationInfos, Generator generator,
            Reporter subReporter) {
        Terminal regulatingTerminal = generator.getRegulatingTerminal();

        String oldVoltageLevel = null;
        String oldEquipment = null;
        // If there is no regulating terminal in file, regulating terminal voltage level
        // is equal to generator voltage level
        if (regulatingTerminal != null
                && !regulatingTerminal.getVoltageLevel().equals(generator.getTerminal().getVoltageLevel())) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }

        if (modificationInfos.getRegulatingTerminalId() != null
                && modificationInfos.getRegulatingTerminalType() != null
                && modificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = getTerminalFromIdentifiable(generator.getNetwork(),
                    modificationInfos.getRegulatingTerminalId().getValue(),
                    modificationInfos.getRegulatingTerminalType().getValue(),
                    modificationInfos.getRegulatingTerminalVlId().getValue());
            generator.setRegulatingTerminal(terminal);

            addModificationReport(oldVoltageLevel,
                    modificationInfos.getRegulatingTerminalVlId().getValue(), subReporter,
                    "Voltage level");
            addModificationReport(oldEquipment,
                    modificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + modificationInfos.getRegulatingTerminalId().getValue(),
                    subReporter,
                    "Equipment");
        }

        // if the voltageRegulationType is set to LOCAL, we set the regulatingTerminal
        // to null
        if (modificationInfos.getVoltageRegulationType() != null
                && modificationInfos.getVoltageRegulationType().getValue() == VoltageRegulationType.LOCAL) {
            generator.setRegulatingTerminal(null);
            addModificationReport(oldVoltageLevel,
                    null, subReporter,
                    "Voltage level");
            addModificationReport(oldEquipment,
                    null,
                    subReporter,
                    "Equipment");
        }
    }

    private void modifyGeneratorVoltageRegulatorAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        // if no modification were done to VoltageRegulatorOn, we get the old value
        Boolean isVoltageRegulationOn = null;
        if (modificationInfos.getVoltageRegulationOn() != null) {
            isVoltageRegulationOn = modificationInfos.getVoltageRegulationOn().getValue();
            applyElementaryModifications(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                    modificationInfos.getVoltageRegulationOn(), subReporter, "Voltage regulation on");
        } else {
            isVoltageRegulationOn = generator.isVoltageRegulatorOn();
        }

        // if voltageRegulationOn is true, we apply modifications to regulatingTerminal
        // and QPercent
        // otherwise we apply modifications to the reactivepower setpoint
        if (Boolean.TRUE.equals(isVoltageRegulationOn)) {
            modifyGeneratorRegulatingTerminal(modificationInfos, generator, subReporter);
            if (modificationInfos.getQPercent() != null) {
                CoordinatedReactiveControl coordinatedReactiveControl = generator
                        .getExtension(CoordinatedReactiveControl.class);
                generator.newExtension(CoordinatedReactiveControlAdderImpl.class)
                        .withQPercent(modificationInfos.getQPercent().getValue())
                        .add();
                addModificationReport(
                        coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent() : Double.NaN,
                        modificationInfos.getQPercent().getValue(),
                        subReporter, "Reactive percentage");
            }
        } else {
            if (modificationInfos.getReactivePowerSetpoint() != null) {
                applyElementaryModifications(generator::setTargetQ, generator::getTargetQ,
                        modificationInfos.getReactivePowerSetpoint(), subReporter, "Reactive power set point");
            }
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
        applyElementaryModifications(generator::setMaxP, generator::getMaxP, modificationInfos.getMaxActivePower(), subReporter, "Max active power");
        applyElementaryModifications(generator::setMinP, generator::getMinP, modificationInfos.getMinActivePower(), subReporter, "Min active power");
        applyElementaryModifications(generator::setRatedS, generator::getRatedS, modificationInfos.getRatedNominalPower(), subReporter, "Rated nominal power");
        applyElementaryModifications(generator::setTargetP, generator::getTargetP, modificationInfos.getActivePowerSetpoint(), subReporter, "Active power set point");
        applyElementaryModifications(generator::setTargetV, generator::getTargetV, modificationInfos.getVoltageSetpoint(), subReporter, "Voltage set point");
        modifyGeneratorVoltageRegulatorAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorShortCircuitAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorActivePowerControlAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorReactiveLimitsAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorStartUpAttributes(modificationInfos, generator, subReporter);
    }

    public List<EquipmentModificationInfos> createLoadModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, LoadModificationInfos loadModificationInfos) {
        assertEquipmentModificationInfosOk(loadModificationInfos, MODIFY_LOAD_ERROR);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateLoadModification(listener, loadModificationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    private List<ModificationInfos> execCreateGeneratorModification(NetworkStoreListener listener,
                                                        GeneratorModificationInfos generatorModificationInfos,
                                                        UUID repordId, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.GENERATOR_MODIFICATION.name(), "Generator modification ${generatorId}", "generatorId", generatorModificationInfos.getEquipmentId());

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                try {
                    Generator generator = getGenerator(network, generatorModificationInfos.getEquipmentId());
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
        }, MODIFY_GENERATOR_ERROR, repordId, reporter, subReporter);
    }

    public List<EquipmentModificationInfos> createGeneratorModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, GeneratorModificationInfos generatorModificationInfo) {
        assertEquipmentModificationInfosOk(generatorModificationInfo, MODIFY_GENERATOR_ERROR);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateGeneratorModification(listener, generatorModificationInfo, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    private void assertEquipmentModificationInfosOk(BasicEquipmentModificationInfos equipmentModificationInfos, NetworkModificationException.Type type) {
        if (equipmentModificationInfos == null) {
            throw new NetworkModificationException(type, "Missing required attributes to modify the equipment");
        }
    }

    private List<ModificationInfos> execCreateEquipmentDeletion(NetworkStoreListener listener,
                                                                EquipmentDeletionInfos equipmentDeletionInfos,
                                                                UUID reportUuid, String reporterId) {
        String equipmentId = equipmentDeletionInfos.getEquipmentId();
        String equipmentType = equipmentDeletionInfos.getEquipmentType();
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.EQUIPMENT_DELETION.name(), "Equipment deletion ${equipmentId}", "equipmentId", equipmentId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, equipmentType, equipmentId);
                if (identifiable == null) {
                    throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + equipmentId + " not found or of bad type");
                }

                if (identifiable instanceof Connectable) {
                    new RemoveFeederBay(equipmentId).apply(network, true, reporter);
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
        }, DELETE_EQUIPMENT_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentDeletionInfos> createEquipmentDeletion(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, EquipmentDeletionInfos equipmentDeletionInfos) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateEquipmentDeletion(listener, equipmentDeletionInfos, reportUuid, reporterId)
                .stream().map(EquipmentDeletionInfos.class::cast).collect(Collectors.toList());
    }

    public void updateEquipmentDeletion(UUID modificationUuid, EquipmentDeletionInfos equipmentDeletionInfos) {

        ModificationEntity equipmentDeletionEntity = this.modificationRepository
                .findById(modificationUuid)
                .orElseThrow(() -> new NetworkModificationException(DELETE_EQUIPMENT_ERROR, "Equipment deletion not found"));

        EquipmentDeletionEntity updatedEntity = this.networkModificationRepository.createEquipmentDeletionEntity(
                equipmentDeletionInfos.getEquipmentId(),
                equipmentDeletionInfos.getEquipmentType());
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(equipmentDeletionEntity.getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private void sendReport(UUID reportUuid, ReporterModel reporter) {
        var path = UriComponentsBuilder.fromPath("{reportUuid}")
            .buildAndExpand(reportUuid)
            .toUriString();
        var headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        try {
            reportServerRest.exchange(this.getReportServerURI() + path, HttpMethod.PUT, new HttpEntity<>(objectMapper.writeValueAsString(reporter), headers), ReporterModel.class);
        } catch (JsonProcessingException error) {
            throw new PowsyblException("error creating report", error);
        }
    }

    public void setReportServerRest(RestTemplate reportServerRest) {
        this.reportServerRest = Objects.requireNonNull(reportServerRest, "reportServerRest can't be null");
    }

    private GeneratorAdder createGeneratorAdderInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos) {

        Terminal terminal = getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            generatorCreationInfos.getRegulatingTerminalId(),
            generatorCreationInfos.getRegulatingTerminalType(),
            generatorCreationInfos.getRegulatingTerminalVlId());

        // creating the generator
        GeneratorAdder generatorAdder = voltageLevel.newGenerator()
            .setId(generatorCreationInfos.getEquipmentId())
            .setName(generatorCreationInfos.getEquipmentName())
            .setEnergySource(generatorCreationInfos.getEnergySource())
            .setMinP(generatorCreationInfos.getMinActivePower())
            .setMaxP(generatorCreationInfos.getMaxActivePower())
            .setRatedS(generatorCreationInfos.getRatedNominalPower() != null ? generatorCreationInfos.getRatedNominalPower() : Double.NaN)
            .setTargetP(generatorCreationInfos.getActivePowerSetpoint())
            .setTargetQ(generatorCreationInfos.getReactivePowerSetpoint() != null ? generatorCreationInfos.getReactivePowerSetpoint() : Double.NaN)
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(generatorCreationInfos.getVoltageSetpoint() != null ? generatorCreationInfos.getVoltageSetpoint() : Double.NaN);

        if (terminal != null) {
            generatorAdder.setRegulatingTerminal(terminal);
        }

        return generatorAdder;
    }

    private void addExtensionsToGenerator(GeneratorCreationInfos generatorCreationInfos, Generator generator, VoltageLevel voltageLevel) {
        Terminal terminal = getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                generatorCreationInfos.getRegulatingTerminalId(),
                generatorCreationInfos.getRegulatingTerminalType(),
                generatorCreationInfos.getRegulatingTerminalVlId());

        if (terminal != null) {
            generator.setRegulatingTerminal(terminal);
        }

        Boolean participate = generatorCreationInfos.getParticipate();

        if (generatorCreationInfos.getPlannedActivePowerSetPoint() != null
                || generatorCreationInfos.getStartupCost() != null
                || generatorCreationInfos.getMarginalCost() != null
                || generatorCreationInfos.getPlannedOutageRate() != null
                || generatorCreationInfos.getForcedOutageRate() != null) {
            generator.newExtension(GeneratorStartupAdderImpl.class)
                    .withPlannedActivePowerSetpoint(generatorCreationInfos.getPlannedActivePowerSetPoint() != null ? generatorCreationInfos.getPlannedActivePowerSetPoint() : Double.NaN)
                    .withStartupCost(generatorCreationInfos.getStartupCost() != null ? generatorCreationInfos.getStartupCost() : Double.NaN)
                    .withMarginalCost(generatorCreationInfos.getMarginalCost() != null ? generatorCreationInfos.getMarginalCost() : Double.NaN)
                    .withPlannedOutageRate(generatorCreationInfos.getPlannedOutageRate() != null ? generatorCreationInfos.getPlannedOutageRate() : Double.NaN)
                    .withForcedOutageRate(generatorCreationInfos.getForcedOutageRate() != null ? generatorCreationInfos.getForcedOutageRate() : Double.NaN)
                    .add();
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

        if (Boolean.TRUE.equals(generatorCreationInfos.getReactiveCapabilityCurve())) {
            ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
            generatorCreationInfos.getReactiveCapabilityCurvePoints()
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

        if (generatorCreationInfos.getQPercent() != null) {
            generator.newExtension(CoordinatedReactiveControlAdderImpl.class).withQPercent(generatorCreationInfos.getQPercent())
                    .add();
        }
    }

    private Generator createGeneratorInBusBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId());

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

        if (generatorCreationInfos.getPlannedActivePowerSetPoint() != null
                || generatorCreationInfos.getStartupCost() != null
                || generatorCreationInfos.getMarginalCost() != null
                || generatorCreationInfos.getPlannedOutageRate() != null
                || generatorCreationInfos.getForcedOutageRate() != null) {
            generator.newExtension(GeneratorStartupAdderImpl.class)
                    .withPlannedActivePowerSetpoint(generatorCreationInfos.getPlannedActivePowerSetPoint() != null ? generatorCreationInfos.getPlannedActivePowerSetPoint() : Double.NaN)
                    .withStartupCost(generatorCreationInfos.getStartupCost() != null ? generatorCreationInfos.getStartupCost() : Double.NaN)
                    .withMarginalCost(generatorCreationInfos.getMarginalCost() != null ? generatorCreationInfos.getMarginalCost() : Double.NaN)
                    .withPlannedOutageRate(generatorCreationInfos.getPlannedOutageRate() != null ? generatorCreationInfos.getPlannedOutageRate() : Double.NaN)
                    .withForcedOutageRate(generatorCreationInfos.getForcedOutageRate() != null ? generatorCreationInfos.getForcedOutageRate() : Double.NaN)
                    .add();
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

        if (Boolean.TRUE.equals(generatorCreationInfos.getReactiveCapabilityCurve())) {
            ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
            generatorCreationInfos.getReactiveCapabilityCurvePoints()
                    .forEach(point -> adder.beginPoint()
                            .setMaxQ(point.getQmaxP())
                            .setMinQ(point.getQminP())
                            .setP(point.getP())
                            .endPoint());
            adder.add();
        }

        return generator;
    }

    private List<ModificationInfos> execCreateGeneratorCreation(NetworkStoreListener listener, GeneratorCreationInfos generatorCreationInfos,
                                                        UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.GENERATOR_CREATION.name(), "Generator creation ${generatorId}", "generatorId", generatorCreationInfos.getEquipmentId());

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the generator in the network
                VoltageLevel voltageLevel = getVoltageLevel(network, generatorCreationInfos.getVoltageLevelId());
                if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    GeneratorAdder generatorAdder = createGeneratorAdderInNodeBreaker(voltageLevel, generatorCreationInfos);
                    var position = generatorCreationInfos.getConnectionPosition() != null ? generatorCreationInfos.getConnectionPosition() :
                            ModificationUtils.getInstance().getPosition(generatorCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);

                    CreateFeederBay algo = new CreateFeederBayBuilder()
                            .withBbsId(generatorCreationInfos.getBusOrBusbarSectionId())
                            .withInjectionDirection(generatorCreationInfos.getConnectionDirection())
                            .withInjectionFeederName(generatorCreationInfos.getConnectionName() != null ? generatorCreationInfos.getConnectionName() : generatorCreationInfos.getEquipmentId())
                            .withInjectionPositionOrder(position)
                            .withInjectionAdder(generatorAdder)
                            .build();

                    algo.apply(network, true, subReporter);

                    // CreateFeederBayBuilder already create the generator using (withInjectionAdder(generatorAdder)) so then we can add extensions
                    var generator = getGenerator(network, generatorCreationInfos.getEquipmentId());
                    addExtensionsToGenerator(generatorCreationInfos, generator, voltageLevel);
                } else {
                    createGeneratorInBusBreaker(voltageLevel, generatorCreationInfos);
                    subReporter.report(Report.builder()
                            .withKey("generatorCreated")
                            .withDefaultMessage("New generator with id=${id} created")
                            .withValue("id", generatorCreationInfos.getEquipmentId())
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                }
            }
            // add the generator creation entity to the listener
            listener.storeGeneratorCreation(generatorCreationInfos);
        }, CREATE_GENERATOR_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentModificationInfos> createGeneratorCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, GeneratorCreationInfos generatorCreationInfos) {
        assertGeneratorCreationInfosNotEmpty(generatorCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateGeneratorCreation(listener, generatorCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    private void assertGeneratorCreationInfosNotEmpty(GeneratorCreationInfos generatorCreationInfos) {
        if (generatorCreationInfos == null) {
            throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Missing required attributes to create the generator");
        }
    }

    private void setBranchAdderNodeOrBus(BranchAdder<?> branchAdder, VoltageLevel voltageLevel, BranchCreationInfos branchCreationInfos, Side side, boolean withSwitch) {
        String currentBusBarSectionId = (side == Side.ONE) ? branchCreationInfos.getBusOrBusbarSectionId1() : branchCreationInfos.getBusOrBusbarSectionId2();

        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
            // busId is a busbar section id
            BusbarSection busbarSection = nodeBreakerView.getBusbarSection(currentBusBarSectionId);
            if (busbarSection == null) {
                throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, currentBusBarSectionId);
            }
            if (withSwitch) {
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
            }
        } else { // BUS BREAKER
            // busId is a bus id
            Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, currentBusBarSectionId);

            // complete the lineAdder
            if (side == Side.ONE) {
                branchAdder.setBus1(bus.getId()).setConnectableBus1(bus.getId());
            } else {
                branchAdder.setBus2(bus.getId()).setConnectableBus2(bus.getId());
            }
        }

    }

    private List<ModificationInfos> execCreateTwoWindingsTransformerCreation(NetworkStoreListener listener, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos,
                                                                     UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION.name(), "Two windings transformer creation ${twoWindingsTransformerId}", "twoWindingsTransformerId", twoWindingsTransformerCreationInfos.getEquipmentId());

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the 2wt in the network
                VoltageLevel voltageLevel1 = getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId1());
                VoltageLevel voltageLevel2 = getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId2());
                if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(network, voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, false, false);

                    var position1 = twoWindingsTransformerCreationInfos.getConnectionPosition1() != null ? twoWindingsTransformerCreationInfos.getConnectionPosition1() : ModificationUtils.getInstance().getPosition(twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
                    var position2 = twoWindingsTransformerCreationInfos.getConnectionPosition2() != null ? twoWindingsTransformerCreationInfos.getConnectionPosition2() : ModificationUtils.getInstance().getPosition(twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

                    CreateBranchFeederBays algo = new CreateBranchFeederBaysBuilder()
                            .withBbsId1(twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId1())
                            .withBbsId2(twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId2())
                            .withFeederName1(twoWindingsTransformerCreationInfos.getConnectionName1() != null ? twoWindingsTransformerCreationInfos.getConnectionName1() : twoWindingsTransformerCreationInfos.getEquipmentId())
                            .withFeederName2(twoWindingsTransformerCreationInfos.getConnectionName2() != null ? twoWindingsTransformerCreationInfos.getConnectionName2() : twoWindingsTransformerCreationInfos.getEquipmentId())
                            .withDirection1(twoWindingsTransformerCreationInfos.getConnectionDirection1())
                            .withDirection2(twoWindingsTransformerCreationInfos.getConnectionDirection2())
                            .withPositionOrder1(position1)
                            .withPositionOrder2(position2)
                            .withBranchAdder(twoWindingsTransformerAdder).build();
                    algo.apply(network, true, subReporter);

                    var twt = network.getTwoWindingsTransformer(twoWindingsTransformerCreationInfos.getEquipmentId());
                    addTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt);
                } else {
                    addTwoWindingsTransformer(network, voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, true, true, subReporter);
                }
            }
            // add the 2wt creation entity to the listener
            listener.storeTwoWindingsTransformerCreation(twoWindingsTransformerCreationInfos);
        }, CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, reportUuid, reporter, subReporter);
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt) {
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            RatioTapChangerCreationInfos ratioTapChangerInfos = twoWindingsTransformerCreationInfos.getRatioTapChanger();
            RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
            Terminal terminal = getTerminalFromIdentifiable(network,
                    ratioTapChangerInfos.getRegulatingTerminalId(),
                    ratioTapChangerInfos.getRegulatingTerminalType(),
                    ratioTapChangerInfos.getRegulatingTerminalVlId());

            if (ratioTapChangerInfos.isRegulating()) {
                ratioTapChangerAdder.setTargetV(ratioTapChangerInfos.getTargetV())
                        .setTargetDeadband(ratioTapChangerInfos.getTargetDeadband() != null ? ratioTapChangerInfos.getTargetDeadband() : 0.)
                        .setRegulationTerminal(terminal);
            }

            ratioTapChangerAdder.setRegulating(ratioTapChangerInfos.isRegulating())
                    .setLoadTapChangingCapabilities(ratioTapChangerInfos.isLoadTapChangingCapabilities())
                    .setLowTapPosition(ratioTapChangerInfos.getLowTapPosition())
                    .setTapPosition(ratioTapChangerInfos.getTapPosition());

            if (ratioTapChangerInfos.getSteps() != null) {
                for (TapChangerStepCreationInfos step : ratioTapChangerInfos.getSteps()) {
                    ratioTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
                }

                ratioTapChangerAdder.add();
            }
        }

        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            PhaseTapChangerCreationInfos phaseTapChangerInfos = twoWindingsTransformerCreationInfos.getPhaseTapChanger();
            PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
            Terminal terminal = getTerminalFromIdentifiable(network,
                    phaseTapChangerInfos.getRegulatingTerminalId(),
                    phaseTapChangerInfos.getRegulatingTerminalType(),
                    phaseTapChangerInfos.getRegulatingTerminalVlId());

            if (phaseTapChangerInfos.isRegulating()) {
                phaseTapChangerAdder.setRegulationValue(phaseTapChangerInfos.getRegulationValue())
                        .setTargetDeadband(phaseTapChangerInfos.getTargetDeadband() != null ? phaseTapChangerInfos.getTargetDeadband() : 0.)
                        .setRegulationTerminal(terminal);
            }

            phaseTapChangerAdder.setRegulating(phaseTapChangerInfos.isRegulating())
                    .setRegulationMode(phaseTapChangerInfos.getRegulationMode())
                    .setLowTapPosition(phaseTapChangerInfos.getLowTapPosition())
                    .setTapPosition(phaseTapChangerInfos.getTapPosition());

            if (phaseTapChangerInfos.getSteps() != null) {
                for (TapChangerStepCreationInfos step : phaseTapChangerInfos.getSteps()) {
                    phaseTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
                }

                phaseTapChangerAdder.add();
            }
        }
    }

    private void addTwoWindingsTransformer(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2, Reporter subReporter) {
        var twt = createTwoWindingsTransformerAdder(network, voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, withSwitch1, withSwitch2).add();
        addTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt);
        subReporter.report(Report.builder()
                .withKey("twoWindingsTransformerCreated")
                .withDefaultMessage("New two windings transformer with id=${id} created")
                .withValue("id", twoWindingsTransformerCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    public List<EquipmentModificationInfos> createTwoWindingsTransformerCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        assertTwoWindingsTransformerCreationInfosNotEmpty(twoWindingsTransformerCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateTwoWindingsTransformerCreation(listener, twoWindingsTransformerCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    private TwoWindingsTransformerAdder createTwoWindingsTransformerAdder(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2) {
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
        TwoWindingsTransformerAdder twoWindingsTransformerAdder = branchAdder.setId(twoWindingsTransformerCreationInfos.getEquipmentId())
                .setName(twoWindingsTransformerCreationInfos.getEquipmentName())
                .setVoltageLevel1(twoWindingsTransformerCreationInfos.getVoltageLevelId1())
                .setVoltageLevel2(twoWindingsTransformerCreationInfos.getVoltageLevelId2())
                .setG(twoWindingsTransformerCreationInfos.getMagnetizingConductance())
                .setB(twoWindingsTransformerCreationInfos.getMagnetizingSusceptance())
                .setR(twoWindingsTransformerCreationInfos.getSeriesResistance())
                .setX(twoWindingsTransformerCreationInfos.getSeriesReactance())
                .setRatedU1(twoWindingsTransformerCreationInfos.getRatedVoltage1())
                .setRatedU2(twoWindingsTransformerCreationInfos.getRatedVoltage2());

        if (twoWindingsTransformerCreationInfos.getRatedS() != null) {
            twoWindingsTransformerAdder.setRatedS(twoWindingsTransformerCreationInfos.getRatedS());
        }

        // BranchAdder completion by topology
        setBranchAdderNodeOrBus(branchAdder, voltageLevel1, twoWindingsTransformerCreationInfos, Side.ONE, withSwitch1);
        setBranchAdderNodeOrBus(branchAdder, voltageLevel2, twoWindingsTransformerCreationInfos, Side.TWO, withSwitch2);

        return twoWindingsTransformerAdder;
    }

    public void updateTwoWindingsTransformerCreation(UUID modificationUuid, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        assertTwoWindingsTransformerCreationInfosNotEmpty(twoWindingsTransformerCreationInfos);
        Optional<ModificationEntity> twoWindingsTransformerModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!twoWindingsTransformerModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Two windings transformer creation not found");
        }
        EquipmentCreationEntity updatedEntity = TwoWindingsTransformerCreationEntity.toEntity(twoWindingsTransformerCreationInfos);
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(twoWindingsTransformerModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private void assertTwoWindingsTransformerCreationInfosNotEmpty(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        if (twoWindingsTransformerCreationInfos == null) {
            throw new NetworkModificationException(CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Missing required attributes to create the two windings transformer");
        }
    }

    private List<ModificationInfos> execCreateSubstationCreation(NetworkStoreListener listener,
                                                                          SubstationCreationInfos substationCreationInfos,
                                                                          UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter(ModificationType.SUBSTATION_CREATION.name(), "Substation creation ${substationId}", "substationId", substationCreationInfos.getEquipmentId());

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                Substation substation = network.newSubstation()
                    .setId(substationCreationInfos.getEquipmentId())
                    .setName(substationCreationInfos.getEquipmentName())
                    .setCountry(substationCreationInfos.getSubstationCountry())
                    .add();
                //substation.setProperty()
                Map<String, String> properties = substationCreationInfos.getProperties();
                if (properties != null) {
                    properties.forEach(substation::setProperty);
                }

                subReporter.report(Report.builder()
                    .withKey("substationCreated")
                    .withDefaultMessage("New substation with id=${id} created")
                    .withValue("id", substationCreationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }

            // add the substation creation entity to the listener
            listener.storeSubstationCreation(substationCreationInfos);
        }, CREATE_SUBSTATION_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentModificationInfos> createSubstationCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, SubstationCreationInfos substationCreationInfos) {
        assertSubstationCreationInfosNotEmpty(substationCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateSubstationCreation(listener, substationCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    public void updateSubstationCreation(UUID modificationUuid, SubstationCreationInfos substationCreationInfos) {
        Optional<ModificationEntity> substationModificationEntity = this.modificationRepository.findById(modificationUuid);

        if (!substationModificationEntity.isPresent()) {
            throw new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Substation creation not found");
        }

        EquipmentCreationEntity updatedEntity = this.networkModificationRepository.createSubstationEntity(substationCreationInfos.getEquipmentId(),
            substationCreationInfos.getEquipmentName(), substationCreationInfos.getSubstationCountry(), substationCreationInfos.getProperties());
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(substationModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private void assertSubstationCreationInfosNotEmpty(SubstationCreationInfos substationCreationInfos) {
        if (substationCreationInfos == null) {
            throw new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Missing required attributes to create the substation");
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

    @Transactional(readOnly = true)
    public List<ModificationInfos> applyModifications(Network network, UUID networkUuid, BuildInfos buildInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(network, networkUuid, null, networkModificationRepository, equipmentInfosService, true, true);

        // Apply all modifications belonging to the modification groups uuids in buildInfos
        final List<ModificationInfos> allModificationsInfos = Streams.zip(buildInfos.getModificationGroupUuids().stream(), buildInfos.getReporterIds().stream(),
            (groupUuid, reporterId) -> {
                List<ModificationInfos> modifications = List.of();
                Stream<ModificationInfos> resultModifications = Stream.of();
                try {
                    modifications = networkModificationRepository.getModificationsInfos(List.of(groupUuid));
                } catch (NetworkModificationException e) {
                    if (e.getType() != MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                        throw e;
                    }
                }

                if (modifications.isEmpty()) {
                    sendReport(buildInfos.getReportUuid(), new ReporterModel(reporterId, reporterId));
                } else {
                    resultModifications = modifications.stream().flatMap(infos -> applyModification(listener, buildInfos.getModificationsToExclude(), groupUuid, buildInfos.getReportUuid(), reporterId, infos).stream());
                }

                return resultModifications;
            }
        ).flatMap(Function.identity()).collect(Collectors.toList());

        // flushing network (only once at the end)
        networkStoreService.flush(listener.getNetwork());

        return allModificationsInfos;
    }

    public void applyModifications(List<UUID> modificationsUuidList, UUID groupUuid, UUID networkUuid, UUID reportUuid, UUID reporterId, String variantId) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, true, networkInfos.isApplyModifications());

        for (UUID modification : modificationsUuidList) {
            ModificationInfos modificationInfos = networkModificationRepository.getModificationInfo(modification);
            applyModification(listener, Set.of(), groupUuid, reportUuid, reporterId.toString(), modificationInfos);
        }

        networkStoreService.flush(listener.getNetwork());
    }

    private List<ModificationInfos> applyModification(NetworkStoreListener listener, Set<UUID> modificationsToExclude, UUID groupUuid,
                                                      UUID reportUuid, String reporterId, ModificationInfos infos) {
        try {
            if (modificationsToExclude.contains(infos.getUuid())) {
                return List.of();
            }

            switch (infos.getType()) {
                case EQUIPMENT_ATTRIBUTE_MODIFICATION:
                case LOAD_CREATION:
                case LINE_SPLIT_WITH_VOLTAGE_LEVEL:
                case DELETE_VOLTAGE_LEVEL_ON_LINE:
                case DELETE_ATTACHING_LINE:
                case SHUNT_COMPENSATOR_CREATION:
                case LINE_CREATION:
                case LINE_ATTACH_TO_VOLTAGE_LEVEL:
                case VOLTAGE_LEVEL_CREATION:
                case LINES_ATTACH_TO_SPLIT_LINES:
                    // Generic form
                    return handleModification(infos, listener, groupUuid, reportUuid, reporterId);

                case LOAD_MODIFICATION:
                    LoadModificationInfos loadModificationInfos = (LoadModificationInfos) infos;
                    return execCreateLoadModification(listener, loadModificationInfos, reportUuid, reporterId);

                case GENERATOR_CREATION:
                    GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) infos;
                    return execCreateGeneratorCreation(listener, generatorCreationInfos, reportUuid, reporterId);

                case GENERATOR_MODIFICATION:
                    var generatorModificationInfos = (GeneratorModificationInfos) infos;
                    return execCreateGeneratorModification(listener, generatorModificationInfos, reportUuid, reporterId);

                case TWO_WINDINGS_TRANSFORMER_CREATION:
                    TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) infos;
                    return execCreateTwoWindingsTransformerCreation(listener, twoWindingsTransformerCreationInfos, reportUuid, reporterId);

                case EQUIPMENT_DELETION:
                    EquipmentDeletionInfos deletionInfos = (EquipmentDeletionInfos) infos;
                    return execCreateEquipmentDeletion(listener, deletionInfos, reportUuid, reporterId);

                case GROOVY_SCRIPT:
                    GroovyScriptModificationInfos groovyModificationInfos = (GroovyScriptModificationInfos) infos;
                    return execCreateGroovyScript(listener, groovyModificationInfos.getScript(), reportUuid, reporterId);

                case SUBSTATION_CREATION:
                    SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) infos;
                    return execCreateSubstationCreation(listener, substationCreationInfos, reportUuid, reporterId);

                case BRANCH_STATUS:
                    BranchStatusModificationInfos branchStatusModificationInfos = (BranchStatusModificationInfos) infos;
                    return execCreateBranchStatusModification(listener, branchStatusModificationInfos, reportUuid, reporterId);

                default:
            }
        } catch (PowsyblException e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(MODIFICATION_ERROR, e);
            ReporterModel reporter = new ReporterModel(reporterId, "Building node");
            reporter.report(Report.builder()
                .withKey(MODIFICATION_ERROR.name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
            sendReport(reportUuid, reporter);
        }

        return List.of();
    }

    /*
    ** Build variant : sending message to rabbitmq
     */
    public void buildVariant(UUID networkUuid, BuildInfos buildInfos, String receiver) {
        notificationService.emitBuildMessage(new BuildExecContext(networkUuid, buildInfos, receiver).toMessage(objectMapper));
    }

    public void stopBuild(String receiver) {
        notificationService.emitCancelBuildMessage(receiver);
    }

    public void deleteNetworkModifications(UUID groupUuid, List<UUID> modificationsUuids) {
        if (networkModificationRepository.deleteModifications(groupUuid, modificationsUuids) == 0) {
            throw new NetworkModificationException(MODIFICATION_NOT_FOUND);
        }
    }

    public void moveModifications(UUID groupUuid, UUID originGroupUuid, UUID before, UUID networkUuid, UUID reportUuid, UUID reporterId, String variantId, List<UUID> modificationsToMove, boolean canBuildNode) {
        List<UUID> movedModifications = networkModificationRepository.moveModifications(groupUuid, originGroupUuid, modificationsToMove, before).getModificationsMoved();
        if (canBuildNode && !movedModifications.isEmpty()) {
            // try to apply the moved modifications (incremental mode)
            applyModifications(movedModifications, groupUuid, networkUuid, reportUuid, reporterId, variantId);
        }
    }

    public void createModificationGroup(UUID sourceGroupUuid, UUID groupUuid) {
        try {
            networkModificationRepository.saveModifications(groupUuid, networkModificationRepository.cloneModificationsEntities(sourceGroupUuid));
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                return;
            }
            throw e;
        }
    }

    // This function cannot be @Transactional because we clone all modifications resetting their id to null,
    // which is not allowed by JPA if we still stay in the same Tx.
    public List<UUID> duplicateModifications(UUID targetGroupUuid, UUID networkUuid, UUID reportUuid, UUID reporterId, String variantId, List<UUID> modificationsUuidList) {
        List<ModificationEntity> duplicatedModificationEntityList = new ArrayList<>();
        List<UUID> missingModificationList = new ArrayList<>();
        for (UUID modifyId : modificationsUuidList) {
            networkModificationRepository.cloneModificationEntity(modifyId).ifPresentOrElse(
                duplicatedModificationEntityList::add,
                () -> missingModificationList.add(modifyId)  // data no more available
            );
        }
        if (!duplicatedModificationEntityList.isEmpty()) {
            networkModificationRepository.saveModifications(targetGroupUuid, duplicatedModificationEntityList);
            // try to apply the duplicated modifications (incremental mode)
            List<UUID> duplicatedModificationList = duplicatedModificationEntityList.stream().map(ModificationEntity::getId).collect(Collectors.toList());
            applyModifications(duplicatedModificationList, targetGroupUuid, networkUuid, reportUuid, reporterId, variantId);
        }
        return missingModificationList;
    }

    public void updateGeneratorModification(UUID modificationUuid, GeneratorModificationInfos generatorModificationInfos) {
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
