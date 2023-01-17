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
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.Branch.Side;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos.ActionType;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ModificationNetworkInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.ModificationApplicator;
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
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

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
                try {
                    saveModifications(listener, groupUuid, modificationInfos.toEntity());
                } catch (Exception e) {
                    throw new PowsyblException(e);
                }
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
            case BRANCH_STATUS:
                return createLineStatusModification(networkUuid, variantId, groupUuid, reportUuid, reporterId, (BranchStatusModificationInfos) modificationInfos);
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

    @Transactional
    public void updateNetworkModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
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
                case LOAD_MODIFICATION:
                case EQUIPMENT_DELETION:
                case GROOVY_SCRIPT:
                case GENERATOR_CREATION:
                case GENERATOR_MODIFICATION:
                case SUBSTATION_CREATION:
                case TWO_WINDINGS_TRANSFORMER_CREATION:
                    // Generic form
                    return handleModification(infos, listener, groupUuid, reportUuid, reporterId);

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
