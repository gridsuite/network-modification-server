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
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LinesAttachToSplitLinesEntity;
import org.gridsuite.modification.server.modifications.ModificationApplicator;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
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

    public NetworkModificationService(@Value("${backing-services.report-server.base-uri:http://report-server}") String reportServerURI,
                                      NetworkStoreService networkStoreService, NetworkModificationRepository networkModificationRepository,
                                      @Lazy EquipmentInfosService equipmentInfosService,
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

    public List<ModificationInfos> createGroovyScript(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, String groovyScript) {
        assertGroovyScriptNotEmpty(groovyScript);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateGroovyScript(listener, groovyScript, reportUuid, reporterId);
    }

    @Transactional
    public List<EquipmentAttributeModificationInfos> createSwitchStateModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, String switchId, boolean open) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder().type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION).equipmentType(IdentifiableType.SWITCH).equipmentId(switchId).equipmentAttributeName("open").equipmentAttributeValue(open).build();
        return handleModification(modificationInfos, listener, groupUuid, reportUuid, reporterId).stream().map(EquipmentAttributeModificationInfos.class::cast).collect(Collectors.toList());
    }

    public List<UUID> getModificationGroups() {
        return networkModificationRepository.getModificationGroupsUuids();
    }

    @Transactional(readOnly = true)
    // Need a transaction for collections lazy loading
    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        return networkModificationRepository.getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound);
    }

    @Transactional(readOnly = true)
    // Need a transaction for collections lazy loading
    public List<ModificationInfos> getModification(UUID modificationUuid) {
        return networkModificationRepository.getModifications(List.of(modificationUuid));
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();
        return terminal1Disconnected && terminal2Disconnected;
    }

    public List<ModificationInfos> createLineStatusModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String lineId, String reporterId, String action) {
        assertBranchActionValid(action);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateBranchStatusModification(listener, lineId, BranchStatusModificationInfos.ActionType.valueOf(action.toUpperCase()), reportUuid, reporterId);
    }

    private List<ModificationInfos> execCreateBranchStatusModification(NetworkStoreListener listener, String lineId, BranchStatusModificationInfos.ActionType action, UUID reportUuid, String reporterId) {
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

    private List<ModificationInfos> execCreateTripLine(NetworkStoreListener listener, String lineId, UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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

    private List<ModificationInfos> execCreateEnergiseLineEnd(NetworkStoreListener listener,
                                                              String lineId,
                                                              Side side,
                                                              UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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

    private List<ModificationInfos> execCreateSwitchOnLine(NetworkStoreListener listener, String lineId, UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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

    private Line getLine(Network network, String lineId) {
        Line line = network.getLine(lineId);
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
        }
        return line;
    }

    private Generator getGenerator(Network network, String generatorId) {
        Generator generator = network.getGenerator(generatorId);
        if (generator == null) {
            throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + generatorId + " does not exist in network");
        }
        return generator;
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

    private void saveModifications(NetworkStoreListener listener, UUID groupUuid, ModificationEntity modificationEntity) {
        networkModificationRepository.saveModifications(groupUuid, List.of(modificationEntity));
        if (listener.isApplyModifications()) {
            networkStoreService.flush(listener.getNetwork());
        }
    }

    // Generic form
    private void updateModification(ModificationInfos modificationInfos, UUID modificationUuid) {
        ModificationEntity modificationEntity = this.modificationRepository.findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)));

        modificationEntity.update(modificationInfos);
    }

    @Transactional
    public List<EquipmentModificationInfos> createLoadCreation(@NonNull UUID networkUuid, String variantId, @NonNull UUID groupUuid, @NonNull UUID reportUuid, @NonNull String reporterId, @NonNull LoadCreationInfos loadCreationInfos) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return handleModification(loadCreationInfos, listener, groupUuid, reportUuid, reporterId).stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    @Transactional
    public void updateLoadCreation(@NonNull LoadCreationInfos loadCreationInfos, @NonNull UUID modificationUuid) {
        updateModification(loadCreationInfos, modificationUuid);
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

    private List<ModificationInfos> execCreateLoadModification(NetworkStoreListener listener,
                                                   LoadModificationInfos loadModificationInfos,
                                                   UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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
        }, MODIFY_LOAD_ERROR, reportUuid, reporter, subReporter);
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
        String subReportId = "Generator modification " + generatorModificationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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
        if (equipmentModificationInfos == null || equipmentModificationInfos.getEquipmentId() == null) {
            throw new NetworkModificationException(type, "Missing required attributes to modify the equipment");
        }
    }

    private List<ModificationInfos> execCreateEquipmentDeletion(NetworkStoreListener listener, String equipmentType, String equipmentId,
                                                        UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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
        }, DELETE_EQUIPMENT_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentDeletionInfos> createEquipmentDeletion(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, String equipmentType, String equipmentId) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateEquipmentDeletion(listener, equipmentType, equipmentId, reportUuid, reporterId)
            .stream().map(EquipmentDeletionInfos.class::cast).collect(Collectors.toList());
    }

    public void updateEquipmentDeletion(UUID modificationUuid, String equipmentType, String equipmentId) {

        ModificationEntity equipmentDeletionEntity = this.modificationRepository
                .findById(modificationUuid)
                .orElseThrow(() -> new NetworkModificationException(DELETE_EQUIPMENT_ERROR, "Equipment deletion not found"));

        EquipmentDeletionEntity updatedEntity = this.networkModificationRepository.createEquipmentDeletionEntity(
                equipmentId,
                equipmentType);
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
        String subReportId = "Generator creation " + generatorCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

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

    private LineAdder createLineAdder(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2) {

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
        setBranchAdderNodeOrBus(lineAdder, voltageLevel1, lineCreationInfos, Side.ONE, withSwitch1);
        setBranchAdderNodeOrBus(lineAdder, voltageLevel2, lineCreationInfos, Side.TWO, withSwitch2);

        return lineAdder;
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
                lineCreationInfos.getCurrentLimits2().getPermanentLimit(),
                lineCreationInfos.getConnectionName1(),
                lineCreationInfos.getConnectionDirection1(),
                lineCreationInfos.getConnectionName2(),
                lineCreationInfos.getConnectionDirection2());
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(lineModificationEntity.get().getGroup());
        this.networkModificationRepository.updateModification(updatedEntity);
    }

    private List<ModificationInfos> execCreateLineCreation(NetworkStoreListener listener, LineCreationInfos lineCreationInfos,
                                                   UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        String subReportId = "Line creation " + lineCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the line in the network
                VoltageLevel voltageLevel1 = getVoltageLevel(network, lineCreationInfos.getVoltageLevelId1());
                VoltageLevel voltageLevel2 = getVoltageLevel(network, lineCreationInfos.getVoltageLevelId2());

                if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                        voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    LineAdder lineAdder = createLineAdder(network, voltageLevel1, voltageLevel2, lineCreationInfos, false, false);
                    var position1 = ModificationUtils.getInstance().getPosition(lineCreationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
                    var position2 = ModificationUtils.getInstance().getPosition(lineCreationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

                    CreateBranchFeederBays algo = new CreateBranchFeederBaysBuilder()
                            .withBbsId1(lineCreationInfos.getBusOrBusbarSectionId1())
                            .withBbsId2(lineCreationInfos.getBusOrBusbarSectionId2())
                            .withFeederName1(lineCreationInfos.getConnectionName1() != null ? lineCreationInfos.getConnectionName1() : lineCreationInfos.getEquipmentId())
                            .withFeederName2(lineCreationInfos.getConnectionName2() != null ? lineCreationInfos.getConnectionName2() : lineCreationInfos.getEquipmentId())
                            .withDirection1(lineCreationInfos.getConnectionDirection1())
                            .withDirection2(lineCreationInfos.getConnectionDirection2())
                            .withPositionOrder1(position1)
                            .withPositionOrder2(position2)
                            .withBranchAdder(lineAdder).build();
                    algo.apply(network, true, subReporter);
                } else {
                    addLine(network, voltageLevel1, voltageLevel2, lineCreationInfos, true, true, subReporter);
                }

                // Set Permanent Current Limits if exist
                CurrentLimitsInfos currentLimitsInfos1 = lineCreationInfos.getCurrentLimits1();
                CurrentLimitsInfos currentLimitsInfos2 = lineCreationInfos.getCurrentLimits2();
                var line = getLine(network, lineCreationInfos.getEquipmentId());

                if (currentLimitsInfos1 != null && currentLimitsInfos1.getPermanentLimit() != null) {
                    line.newCurrentLimits1().setPermanentLimit(currentLimitsInfos1.getPermanentLimit()).add();
                }
                if (currentLimitsInfos2 != null && currentLimitsInfos2.getPermanentLimit() != null) {
                    line.newCurrentLimits2().setPermanentLimit(currentLimitsInfos2.getPermanentLimit()).add();
                }
            }
            // add the line creation entity to the listener
            listener.storeLineCreation(lineCreationInfos);
        }, CREATE_LINE_ERROR, reportUuid, reporter, subReporter);
    }

    private void addLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2, Reporter subReporter) {
        createLineAdder(network, voltageLevel1, voltageLevel2, lineCreationInfos, withSwitch1, withSwitch2).add();

        subReporter.report(Report.builder()
                .withKey("lineCreated")
                .withDefaultMessage("New line with id=${id} created")
                .withValue("id", lineCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    public List<EquipmentModificationInfos> createLineCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, LineCreationInfos lineCreationInfos) {
        assertLineCreationInfosNotEmpty(lineCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateLineCreation(listener, lineCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    private void assertLineCreationInfosNotEmpty(LineCreationInfos lineCreationInfos) {
        if (lineCreationInfos == null) {
            throw new NetworkModificationException(CREATE_LINE_ERROR, "Missing required attributes to create the line");
        }
    }

    private List<ModificationInfos> execCreateTwoWindingsTransformerCreation(NetworkStoreListener listener, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos,
                                                                     UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        String subReportId = "Two windings transformer creation " + twoWindingsTransformerCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the 2wt in the network
                VoltageLevel voltageLevel1 = getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId1());
                VoltageLevel voltageLevel2 = getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId2());
                if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(network, voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, false, false);

                    var position1 = ModificationUtils.getInstance().getPosition(twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
                    var position2 = ModificationUtils.getInstance().getPosition(twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

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
                        .setTargetDeadband(ratioTapChangerInfos.getTargetDeadband() != null ? ratioTapChangerInfos.getTargetDeadband() : Double.NaN)
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
                        .setTargetDeadband(phaseTapChangerInfos.getTargetDeadband() != null ? phaseTapChangerInfos.getTargetDeadband() : Double.NaN)
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

    public void updateTwoWindingsTransformerCreation(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, UUID modificationUuid) {
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

    private List<ModificationInfos> execCreateSubstationCreation(NetworkStoreListener listener, SubstationCreationInfos substationCreationInfos,
                                                         UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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
        }, CREATE_SUBSTATION_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentModificationInfos> createSubstationCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, SubstationCreationInfos substationCreationInfos) {
        assertSubstationCreationInfosNotEmpty(substationCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateSubstationCreation(listener, substationCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
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

    private List<ModificationInfos> execCreateVoltageLevelCreation(NetworkStoreListener listener, VoltageLevelCreationInfos voltageLevelCreationInfos,
                                                           UUID reportUuid, String reporterId) {

        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        String subReportId = "VoltageLevel creation " + voltageLevelCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                ModificationUtils.getInstance().createVoltageLevelAction(voltageLevelCreationInfos, subReporter, network);
            }
            listener.storeVoltageLevelCreation(voltageLevelCreationInfos);
        }, CREATE_VOLTAGE_LEVEL_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentModificationInfos> createVoltageLevelCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId,
                                                                       VoltageLevelCreationInfos voltageLevelCreationInfos) {
        assertVoltageLevelCreationInfosNotEmpty(voltageLevelCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid,
                networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateVoltageLevelCreation(listener, voltageLevelCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
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

                case LINE_CREATION:
                    LineCreationInfos lineCreationInfos = (LineCreationInfos) infos;
                    return execCreateLineCreation(listener, lineCreationInfos, reportUuid, reporterId);

                case TWO_WINDINGS_TRANSFORMER_CREATION:
                    TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) infos;
                    return execCreateTwoWindingsTransformerCreation(listener, twoWindingsTransformerCreationInfos, reportUuid, reporterId);

                case EQUIPMENT_DELETION:
                    EquipmentDeletionInfos deletionInfos = (EquipmentDeletionInfos) infos;
                    return execCreateEquipmentDeletion(listener, deletionInfos.getEquipmentType(), deletionInfos.getEquipmentId(), reportUuid, reporterId);

                case GROOVY_SCRIPT:
                    GroovyScriptModificationInfos groovyModificationInfos = (GroovyScriptModificationInfos) infos;
                    return execCreateGroovyScript(listener, groovyModificationInfos.getScript(), reportUuid, reporterId);

                case SUBSTATION_CREATION:
                    SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) infos;
                    return execCreateSubstationCreation(listener, substationCreationInfos, reportUuid, reporterId);

                case VOLTAGE_LEVEL_CREATION:
                    VoltageLevelCreationInfos voltageLevelCreationInfos = (VoltageLevelCreationInfos) infos;
                    return execCreateVoltageLevelCreation(listener, voltageLevelCreationInfos, reportUuid, reporterId);

                case BRANCH_STATUS:
                    BranchStatusModificationInfos branchStatusModificationInfos = (BranchStatusModificationInfos) infos;
                    return execCreateBranchStatusModification(listener, branchStatusModificationInfos.getEquipmentId(), branchStatusModificationInfos.getAction(), reportUuid, reporterId);

                case SHUNT_COMPENSATOR_CREATION:
                    ShuntCompensatorCreationInfos shuntCompensatorCreationInfos = (ShuntCompensatorCreationInfos) infos;
                    return execCreateShuntCompensatorCreation(listener, shuntCompensatorCreationInfos, reportUuid, reporterId);

                case LINE_ATTACH_TO_VOLTAGE_LEVEL:
                    LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos = (LineAttachToVoltageLevelInfos) infos;
                    return execCreateLineAttachToVoltageLevelCreation(listener, lineAttachToVoltageLevelInfos, reportUuid, reporterId);

                case LINES_ATTACH_TO_SPLIT_LINES:
                    LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) infos;
                    return execCreateLinesAttachToSplitLinesCreation(listener, linesAttachToSplitLinesInfos, reportUuid, reporterId);

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

    private ShuntCompensatorAdder createShuntAdderInNodeBreaker(VoltageLevel voltageLevel, ShuntCompensatorCreationInfos shuntCompensatorInfos) {
        // creating the shunt compensator
        ShuntCompensatorAdder shunt = voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getEquipmentId())
                .setName(shuntCompensatorInfos.getEquipmentName())
                .setSectionCount(shuntCompensatorInfos.getCurrentNumberOfSections());

        /* when we create non linear shunt, this is where we branch ;) */
        shunt.newLinearModel()
                .setBPerSection(shuntCompensatorInfos.getSusceptancePerSection())
                .setMaximumSectionCount(shuntCompensatorInfos.getMaximumNumberOfSections()).add();

        return shunt;
    }

    private void createShuntInBusBreaker(VoltageLevel voltageLevel, ShuntCompensatorCreationInfos shuntCompensatorInfos) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, shuntCompensatorInfos.getBusOrBusbarSectionId());
        /* creating the shunt compensator */
        voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getEquipmentId())
                .setName(shuntCompensatorInfos.getEquipmentName())
                .setSectionCount(shuntCompensatorInfos.getCurrentNumberOfSections())
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .newLinearModel()
                .setBPerSection(shuntCompensatorInfos.getSusceptancePerSection())
                .setMaximumSectionCount(shuntCompensatorInfos.getMaximumNumberOfSections())
                .add();
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

    private List<ModificationInfos> execCreateShuntCompensatorCreation(NetworkStoreListener listener, ShuntCompensatorCreationInfos shuntCompensatorCreationInfos,
                                                               UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        String subReportId = "Shunt compensator creation " + shuntCompensatorCreationInfos.getEquipmentId();
        Reporter subReporter = reporter.createSubReporter(subReportId, subReportId);

        return doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                // create the shunt compensator in the network
                VoltageLevel voltageLevel = getVoltageLevel(network, shuntCompensatorCreationInfos.getVoltageLevelId());
                if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                    ShuntCompensatorAdder shuntCompensatorAdder = createShuntAdderInNodeBreaker(voltageLevel, shuntCompensatorCreationInfos);
                    var position = shuntCompensatorCreationInfos.getConnectionPosition() != null ? shuntCompensatorCreationInfos.getConnectionPosition() :
                            ModificationUtils.getInstance().getPosition(shuntCompensatorCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);
                    CreateFeederBay algo = new CreateFeederBayBuilder()
                            .withBbsId(shuntCompensatorCreationInfos.getBusOrBusbarSectionId())
                            .withInjectionDirection(shuntCompensatorCreationInfos.getConnectionDirection())
                            .withInjectionFeederName(shuntCompensatorCreationInfos.getConnectionName())
                            .withInjectionPositionOrder(position)
                            .withInjectionAdder(shuntCompensatorAdder)
                            .build();
                    algo.apply(network, true, subReporter);
                } else {
                    createShuntInBusBreaker(voltageLevel, shuntCompensatorCreationInfos);
                    subReporter.report(Report.builder()
                            .withKey("shuntCompensatorCreated")
                            .withDefaultMessage("New shunt compensator with id=${id} created")
                            .withValue("id", shuntCompensatorCreationInfos.getEquipmentId())
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                }
            }

            // add the shunt compensator creation entity to the listener
            listener.storeShuntCompensatorCreation(shuntCompensatorCreationInfos);
        }, CREATE_SHUNT_COMPENSATOR_ERROR, reportUuid, reporter, subReporter);
    }

    public List<EquipmentModificationInfos> createShuntCompensatorCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        assertShuntCompensatorCreationInfosNotEmpty(shuntCompensatorCreationInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateShuntCompensatorCreation(listener, shuntCompensatorCreationInfos, reportUuid, reporterId)
            .stream().map(EquipmentModificationInfos.class::cast).collect(Collectors.toList());
    }

    public void moveModifications(UUID groupUuid, UUID before, List<UUID> modificationsToMove) {
        networkModificationRepository.moveModifications(groupUuid, modificationsToMove, before);
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
    public List<UUID> duplicateModifications(UUID targetGroupUuid, List<UUID> modificationsToDuplicate) {
        List<ModificationEntity> newModificationList = new ArrayList<>();
        List<UUID> missingModificationList = new ArrayList<>();
        for (UUID modifyId : modificationsToDuplicate) {
            networkModificationRepository.cloneModificationEntity(modifyId).ifPresentOrElse(
                newModificationList::add,
                () -> missingModificationList.add(modifyId)  // data no more available
            );
        }
        networkModificationRepository.saveModifications(targetGroupUuid, newModificationList);

        return missingModificationList;
    }

    @Transactional
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

    @Transactional
    public List<ModificationInfos> createLineSplitWithVoltageLevelCreation(@NonNull UUID networkUuid, String variantId, @NonNull UUID groupUuid, @NonNull UUID reportUuid, @NonNull String reporterId,
                                                                           @NonNull LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return handleModification(lineSplitWithVoltageLevelInfos, listener, groupUuid, reportUuid, reporterId).stream().map(ModificationInfos.class::cast).collect(Collectors.toList());
    }

    @Transactional
    public void updateLineSplitWithVoltageLevelCreation(@NonNull UUID modificationUuid, @NonNull LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        updateModification(lineSplitWithVoltageLevelInfos, modificationUuid);
    }

    private void assertLineAttachToVoltageLevelInfosNotEmpty(LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        if (lineAttachToVoltageLevelInfos == null) {
            throw new NetworkModificationException(LINE_ATTACH_ERROR,
                    "Missing required attributes to attach a line to a voltage level");
        }
    }

    private void assertLinesAttachToSplitLinesInfosNotEmpty(LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        if (linesAttachToSplitLinesInfos == null) {
            throw new NetworkModificationException(LINE_ATTACH_ERROR,
                    "Missing required attributes to attach lines to a split lines");
        }
    }

    private List<ModificationInfos> execCreateLineAttachToVoltageLevelCreation(NetworkStoreListener listener,
                                                                               LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos,
                                                                               UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();
        VoltageLevelCreationInfos mayNewVL = lineAttachToVoltageLevelInfos.getMayNewVoltageLevelInfos();
        LineCreationInfos attachmentLineInfos = lineAttachToVoltageLevelInfos.getAttachmentLine();
        if (attachmentLineInfos == null) {
            throw new NetworkModificationException(LINE_ATTACH_ERROR, "Missing required attachment line description");
        }

        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
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

                CreateLineOnLine algo = new CreateLineOnLine(
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

                algo.apply(network, false, subReporter);
            }

            listener.storeLineAttachToVoltageLevelInfos(lineAttachToVoltageLevelInfos);
        }, LINE_ATTACH_ERROR, reportUuid, reporter, subReporter).stream().map(ModificationInfos.class::cast)
                .collect(Collectors.toList());

        if (!inspectable.isEmpty()) {
            inspectable.addAll(listener.getDeletions());
        }
        return inspectable;
    }

    private List<ModificationInfos> execCreateLinesAttachToSplitLinesCreation(NetworkStoreListener listener,
                                                                              LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos,
                                                                              UUID reportUuid, String reporterId) {
        Network network = listener.getNetwork();

        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = reporter.createSubReporter("linesAttachToSplitLines", "Lines attach to split lines");

        List<ModificationInfos> inspectable = doAction(listener, () -> {
            if (listener.isApplyModifications()) {
                String voltageLevelId = linesAttachToSplitLinesInfos.getVoltageLevelId();
                ReplaceTeePointByVoltageLevelOnLineBuilder builder = new ReplaceTeePointByVoltageLevelOnLineBuilder();
                ReplaceTeePointByVoltageLevelOnLine algo = builder.withLine1ZId(linesAttachToSplitLinesInfos.getLineToAttachTo1Id())
                        .withLineZ2Id(linesAttachToSplitLinesInfos.getLineToAttachTo2Id())
                        .withLineZPId(linesAttachToSplitLinesInfos.getAttachedLineId())
                        .withVoltageLevelId(voltageLevelId)
                        .withBbsOrBusId(linesAttachToSplitLinesInfos.getBbsBusId())
                        .withLine1CId(linesAttachToSplitLinesInfos.getReplacingLine1Id())
                        .withLine1CName(linesAttachToSplitLinesInfos.getReplacingLine1Name())
                        .withLineC2Id(linesAttachToSplitLinesInfos.getReplacingLine2Id())
                        .withLineC2Name(linesAttachToSplitLinesInfos.getReplacingLine2Name())
                        .build();
                algo.apply(network, true, subReporter);
            }

            listener.storeLinesAttachToSplitLinesInfos(linesAttachToSplitLinesInfos);
        }, LINE_NOT_FOUND, reportUuid, reporter, subReporter).stream().map(ModificationInfos.class::cast)
                .collect(Collectors.toList());

        if (!inspectable.isEmpty()) {
            inspectable.addAll(listener.getDeletions());
        }
        return inspectable;
    }

    public List<ModificationInfos> createLineAttachToVoltageLevelCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId,
                                                                          LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        assertLineAttachToVoltageLevelInfosNotEmpty(lineAttachToVoltageLevelInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateLineAttachToVoltageLevelCreation(listener, lineAttachToVoltageLevelInfos, reportUuid, reporterId);
    }

    public List<ModificationInfos> createLinesAttachToSplitLinesCreation(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId,
                                                                         LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        assertLinesAttachToSplitLinesInfosNotEmpty(linesAttachToSplitLinesInfos);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateLinesAttachToSplitLinesCreation(listener, linesAttachToSplitLinesInfos, reportUuid, reporterId);
    }

    public void updateLineAttachToVoltageLevelCreation(UUID modificationUuid, LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
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

    public void updateLinesAttachToSplitLinesCreation(UUID modificationUuid, LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        assertLinesAttachToSplitLinesInfosNotEmpty(linesAttachToSplitLinesInfos);

        Optional<ModificationEntity> linesAttachToSplitLinesEntity = this.modificationRepository.findById(modificationUuid);

        if (linesAttachToSplitLinesEntity.isEmpty()) {
            throw new NetworkModificationException(LINE_ATTACH_NOT_FOUND, "Line attach to split line not found");
        }

        LinesAttachToSplitLinesEntity updatedEntity = LinesAttachToSplitLinesEntity.toEntity(
                linesAttachToSplitLinesInfos.getLineToAttachTo1Id(),
                linesAttachToSplitLinesInfos.getLineToAttachTo2Id(),
                linesAttachToSplitLinesInfos.getAttachedLineId(),
                linesAttachToSplitLinesInfos.getVoltageLevelId(),
                linesAttachToSplitLinesInfos.getBbsBusId(),
                linesAttachToSplitLinesInfos.getReplacingLine1Id(),
                linesAttachToSplitLinesInfos.getReplacingLine1Name(),
                linesAttachToSplitLinesInfos.getReplacingLine2Id(),
                linesAttachToSplitLinesInfos.getReplacingLine2Name()
        );
        updatedEntity.setId(modificationUuid);
        updatedEntity.setGroup(linesAttachToSplitLinesEntity.get().getGroup());
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
