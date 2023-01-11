/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_TYPE_EMPTY;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_TWO_WINDINGS_TRANSFORMER_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.DELETE_EQUIPMENT_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_EMPTY;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_LOAD_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.NETWORK_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.TYPE_MISMATCH;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VARIANT_NOT_FOUND;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BasicEquipmentModificationInfos;
import org.gridsuite.modification.server.dto.BranchCreationInfos;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos.ActionType;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.GroovyScriptModificationInfos;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ModificationNetworkInfos;
import org.gridsuite.modification.server.dto.PhaseTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.RatioTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.TapChangerStepCreationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.EquipmentCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.TwoWindingsTransformerCreationEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.modifications.ModificationApplicator;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Streams;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.ReporterModelDeserializer;
import com.powsybl.commons.reporter.ReporterModelJsonModule;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBays;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBaysBuilder;
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Branch.Side;
import com.powsybl.iidm.network.BranchAdder;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Connectable;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChangerAdder;
import com.powsybl.iidm.network.RatioTapChangerAdder;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.TwoWindingsTransformerAdder;
import com.powsybl.iidm.network.VariantManagerConstants;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import com.powsybl.network.store.client.NetworkStoreService;

import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import lombok.NonNull;

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
    private final ApplicationContext context;

    private static final String REPORT_API_VERSION = "v1";
    private static final String DELIMITER = "/";

    private static final String NETWORK_MODIFICATION_TYPE_REPORT = "NetworkModification";
    private static final String LINE_ID_PARAMETER = "lineId";

    public NetworkModificationService(@Value("${gridsuite.services.report-server.base-uri:http://report-server}") String reportServerURI,
                                      NetworkStoreService networkStoreService, NetworkModificationRepository networkModificationRepository,
                                      EquipmentInfosService equipmentInfosService,
                                      ModificationRepository modificationRepository, NotificationService notificationService,
                                      ModificationApplicator modificationApplicator, ObjectMapper objectMapper,
                                      ApplicationContext context) {
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
        this.context = context;
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

    // Generic form
    private List<ModificationInfos> handleModification(ModificationInfos modificationInfos, NetworkStoreListener listener, UUID groupUuid, UUID reportUuid, String reporterId) {
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        List<ModificationInfos> networkModifications = List.of();
        try {
            if (listener.isApplyModifications()) {
                networkModifications = modificationApplicator.apply(modificationInfos, reporter, listener, context);
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

        ModificationUtils.getInstance().applyElementaryModifications(load::setName, load::getNameOrId, loadModificationInfos.getEquipmentName(), subReporter, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(load::setLoadType, load::getLoadType, loadModificationInfos.getLoadType(), subReporter, "Type");
        ModificationUtils.getInstance().applyElementaryModifications(load::setP0, load::getP0, loadModificationInfos.getActivePower(), subReporter, "Active power");
        ModificationUtils.getInstance().applyElementaryModifications(load::setQ0, load::getQ0, loadModificationInfos.getReactivePower(), subReporter, "Reactive power");

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

    public List<EquipmentModificationInfos> createLoadModification(UUID networkUuid, String variantId, UUID groupUuid, UUID reportUuid, String reporterId, LoadModificationInfos loadModificationInfos) {
        assertEquipmentModificationInfosOk(loadModificationInfos, MODIFY_LOAD_ERROR);
        ModificationNetworkInfos networkInfos = getNetworkModificationInfos(networkUuid, variantId);
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkUuid, groupUuid, networkModificationRepository, equipmentInfosService, false, networkInfos.isApplyModifications());
        return execCreateLoadModification(listener, loadModificationInfos, reportUuid, reporterId)
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
                Identifiable<?> identifiable = ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, equipmentType, equipmentId);
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
                VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId1());
                VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, twoWindingsTransformerCreationInfos.getVoltageLevelId2());
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
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
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
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
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
                case GENERATOR_CREATION:
                case GENERATOR_MODIFICATION:
                case SUBSTATION_CREATION:
                    // Generic form
                    return handleModification(infos, listener, groupUuid, reportUuid, reporterId);

                case LOAD_MODIFICATION:
                    LoadModificationInfos loadModificationInfos = (LoadModificationInfos) infos;
                    return execCreateLoadModification(listener, loadModificationInfos, reportUuid, reporterId);

                case TWO_WINDINGS_TRANSFORMER_CREATION:
                    TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) infos;
                    return execCreateTwoWindingsTransformerCreation(listener, twoWindingsTransformerCreationInfos, reportUuid, reporterId);

                case EQUIPMENT_DELETION:
                    EquipmentDeletionInfos deletionInfos = (EquipmentDeletionInfos) infos;
                    return execCreateEquipmentDeletion(listener, deletionInfos, reportUuid, reporterId);

                case GROOVY_SCRIPT:
                    GroovyScriptModificationInfos groovyModificationInfos = (GroovyScriptModificationInfos) infos;
                    return execCreateGroovyScript(listener, groovyModificationInfos.getScript(), reportUuid, reporterId);

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
}
