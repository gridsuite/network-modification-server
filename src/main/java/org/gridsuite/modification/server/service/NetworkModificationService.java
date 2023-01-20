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
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ModificationNetworkInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.ModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationService.class);

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

    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        networkModificationRepository.deleteModificationGroup(groupUuid, errorOnGroupNotFound);
    }

    private NetworkModificationException handleException(NetworkModificationException.Type typeIfError, Reporter subReporter, Exception e) {
        NetworkModificationException networkModificationException;
        networkModificationException = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(typeIfError, e);
        boolean isApplicationException = PowsyblException.class.isAssignableFrom(e.getClass());
        if (!isApplicationException && LOGGER.isErrorEnabled()) {
            LOGGER.error(e.toString(), e);
        }
        String errorMessage = isApplicationException ? e.getMessage() : "Technical error: " + e;
        subReporter.report(Report.builder()
            .withKey(typeIfError.name())
            .withDefaultMessage(errorMessage)
            .withSeverity(TypedValue.ERROR_SEVERITY)
            .build());
        return networkModificationException;
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

    List<ModificationInfos> handleModification(ModificationInfos modificationInfos, NetworkStoreListener listener, UUID groupUuid,
                                               UUID reportUuid, String reporterId) {
        String rootReporterId = reporterId + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = modificationInfos.createSubReporter(reporter);

        List<ModificationInfos> networkModifications = List.of();
        try {
            if (!listener.isBuild()) { // Save modification action in DB
                networkModificationRepository.saveModifications(groupUuid, List.of(modificationInfos.toEntity()));
            }

            if (listener.isApplyModifications()) { // Apply modification on the network
                networkModifications = modificationApplicator.apply(modificationInfos, subReporter, listener, context);

                if (!listener.isBuild()) { // Save network in DB in incremental mode only
                    networkStoreService.flush(listener.getNetwork());
                }
            }
        } catch (Exception e) {
            NetworkModificationException networkModificationException = handleException(modificationInfos.getErrorType(), subReporter, e);
            if (!listener.isBuild()) {
                throw networkModificationException;
            }
        } finally {
            if (listener.isApplyModifications()) {
                sendReport(reportUuid, reporter);
            }
        }

        return networkModifications;
    }

    @Transactional
    public void updateNetworkModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
        ModificationEntity modificationEntity = this.modificationRepository.findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)));

        modificationEntity.update(modificationInfos);
    }

    // No transactional because we need to save modification in DB also in case of error
    // Transaction made in 'saveModifications' method
    public List<ModificationInfos> createNetworkModification(@NonNull UUID networkUuid, String variantId, @NonNull UUID groupUuid, @NonNull UUID reportUuid, @NonNull String reporterId, @NonNull ModificationInfos modificationInfos) {
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

    private void applyModifications(List<UUID> modificationsUuidList, UUID groupUuid, UUID networkUuid, UUID reportUuid, UUID reporterId, String variantId) {
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

            return handleModification(infos, listener, groupUuid, reportUuid, reporterId);
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
