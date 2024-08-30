/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.google.common.collect.Streams;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;

import jakarta.annotation.PreDestroy;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.NetworkModificationObserver;
import org.gridsuite.modification.server.service.ReportService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationApplicator {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationApplicator.class);

    public static final String NETWORK_MODIFICATION_TYPE_REPORT = "NetworkModification";

    private final NetworkStoreService networkStoreService;

    private final EquipmentInfosService equipmentInfosService;

    private final ReportService reportService;

    @Getter private final FilterService filterService;

    private final ExecutorService applicationExecutor;

    private final NetworkModificationObserver networkModificationObserver;

    @Value("${impacts.collection-threshold:50}")
    @Setter // TODO REMOVE when VoltageInitReportTest will no longer use NetworkModificationApplicator
    private Integer collectionThreshold;

    public NetworkModificationApplicator(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                         ReportService reportService, FilterService filterService,
                                         @Value("${max-large-concurrent-applications}") int maxConcurrentApplications,
                                         NetworkModificationObserver networkModificationObserver) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.reportService = reportService;
        this.filterService = filterService;
        this.applicationExecutor = Executors.newFixedThreadPool(maxConcurrentApplications);
        this.networkModificationObserver = networkModificationObserver;

    }

    /* This method is used when creating, inserting, moving or duplicating modifications
     * Since there is no queue for these operations and they can be memory consuming when the preloading strategy is large
     * (for example for VOLTAGE_INIT_MODIFICATION),
     * we limit the number of concurrent applications of these modifications to avoid out of memory issues.
     * We keep the possibility to apply small or medium modifications immediately in parallel without limits.
     * And if in the future we also need to limit the memory consumption of medium modifications we can add more code here.
     * Note : we currently have 3 sizes of modifications :
     * small : preloadingStrategy = NONE
     * medium : preloadingStrategy = COLLECTION
     * large : preloadingStrategy = ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW
     */
    public NetworkModificationResult applyModifications(List<ModificationInfos> modificationInfosList, NetworkInfos networkInfos, ReportInfos reportInfos) {
        PreloadingStrategy preloadingStrategy = modificationInfosList.stream()
            .map(ModificationInfos::getType)
            .reduce(ModificationType::maxStrategy)
            .map(ModificationType::getStrategy)
            .orElse(PreloadingStrategy.NONE);
        if (preloadingStrategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW) {
            CompletableFuture<NetworkModificationResult> future = CompletableFuture.supplyAsync(() -> processApplication(modificationInfosList, networkInfos, reportInfos), applicationExecutor);
            return future.join();
        } else {
            return processApplication(modificationInfosList, networkInfos, reportInfos);
        }
    }

    // used for creating, inserting, moving or duplicating modifications
    private NetworkModificationResult processApplication(List<ModificationInfos> modificationInfosList, NetworkInfos networkInfos, ReportInfos reportInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService, collectionThreshold);
        ApplicationStatus groupApplicationStatus = apply(modificationInfosList, listener.getNetwork(), reportInfos);
        List<AbstractBaseImpact> networkImpacts = listener.flushNetworkModifications();
        return NetworkModificationResult.builder()
                .applicationStatus(groupApplicationStatus)
                .lastGroupApplicationStatus(groupApplicationStatus)
                .networkImpacts(networkImpacts)
                .build();
    }

    /* This method is used when building a variant
     * building a variant is limited to ${consumer.concurrency} (typically 2) concurrent builds thanks to rabbitmq queue
     * but since the other operations (create, insert, move, duplicate) are not inserted in the same rabbitmq queue
     * we use the same ExecutorService to globally limit the number of concurrent large modifications in order to avoid out of memory issues
     * We keep the possibility to apply small or medium modifications immediately.
     * And if in the future we also need to limit the memory consumption of medium modifications we can add more code here.
     * Note : it is possible that the rabbitmq consumer threads here will be blocked by modifications applied directly in the other applyModifications method
     * and no more builds can go through. If this causes problems we should put them in separate rabbitmq queues.
     */
    public NetworkModificationResult applyModifications(List<Pair<String, List<ModificationInfos>>> modificationInfosGroups, NetworkInfos networkInfos, UUID reportUuid) {
        PreloadingStrategy preloadingStrategy = modificationInfosGroups.stream()
                .map(Pair::getRight)
                .flatMap(List::stream)
                .map(ModificationInfos::getType)
                .reduce(ModificationType::maxStrategy)
                .map(ModificationType::getStrategy)
                .orElse(PreloadingStrategy.NONE);
        if (preloadingStrategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW) {
            CompletableFuture<NetworkModificationResult> future = CompletableFuture.supplyAsync(() -> processApplication(modificationInfosGroups, networkInfos, reportUuid), applicationExecutor);
            return future.join();
        } else {
            return processApplication(modificationInfosGroups, networkInfos, reportUuid);
        }
    }

    // used for building a variant
    private NetworkModificationResult processApplication(List<Pair<String, List<ModificationInfos>>> modificationInfosGroups, NetworkInfos networkInfos, UUID reportUuid) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService, collectionThreshold);
        List<ApplicationStatus> groupsApplicationStatuses =
                modificationInfosGroups.stream()
                        .map(g -> apply(g.getRight(), listener.getNetwork(), new ReportInfos(reportUuid, g.getLeft())))
                        .toList();
        List<AbstractBaseImpact> networkImpacts = listener.flushNetworkModifications();
        return NetworkModificationResult.builder()
                .applicationStatus(groupsApplicationStatuses.stream().reduce(ApplicationStatus::max).orElse(ApplicationStatus.ALL_OK))
                .lastGroupApplicationStatus(Streams.findLast(groupsApplicationStatuses.stream()).orElse(ApplicationStatus.ALL_OK))
                .networkImpacts(networkImpacts)
                .build();
    }

    private ApplicationStatus apply(List<ModificationInfos> modificationInfosList, Network network, ReportInfos reportInfos) {
        ReportNode reportNode;
        if (reportInfos.getReporterId() != null) {
            String rootReporterId = reportInfos.getReporterId() + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
            reportNode = ReportNode.newRootReportNode().withMessageTemplate(rootReporterId, rootReporterId).build();
        } else {
            reportNode = ReportNode.NO_OP;
        }
        ApplicationStatus groupApplicationStatus = modificationInfosList.stream()
                .filter(ModificationInfos::getActive)
                .map(m -> apply(m, network, reportNode))
                .reduce(ApplicationStatus::max)
                .orElse(ApplicationStatus.ALL_OK);
        if (reportInfos.getReportUuid() != null) {
            reportService.sendReport(reportInfos.getReportUuid(), reportNode);
        }
        return groupApplicationStatus;
    }

    private ApplicationStatus apply(ModificationInfos modificationInfos, Network network, ReportNode reportNode) {
        ReportNode subReportNode = modificationInfos.createSubReportNode(reportNode);
        try {
            networkModificationObserver.observe("apply", modificationInfos.getType(), () -> apply(modificationInfos.toModification(), network, subReportNode));
        } catch (Exception e) {
            handleException(modificationInfos.getErrorType(), subReportNode, e);
        }
        return getApplicationStatus(reportNode);
    }

    private void apply(AbstractModification modification, Network network, ReportNode subReportNode) {
        // check input data but don't change the network
        modification.check(network);

        // init application context
        modification.initApplicationContext(this);

        // apply all changes on the network
        modification.apply(network, subReportNode);
    }

    private void handleException(NetworkModificationException.Type typeIfError, ReportNode subReportNode, Exception e) {
        boolean isApplicationException = PowsyblException.class.isAssignableFrom(e.getClass());
        if (!isApplicationException && LOGGER.isErrorEnabled()) {
            LOGGER.error(e.toString(), e);
        }
        String errorMessage = isApplicationException ? e.getMessage() : "Technical error: " + e;

        subReportNode.newReportNode()
                .withMessageTemplate(typeIfError.name(), "${errorMessage}")
                .withTypedValue("typedValue", 20, "type")
                .withUntypedValue("errorMessage", errorMessage)
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .add();
    }

    public static boolean areSeveritiesEquals(TypedValue s1, TypedValue s2) {
        return s1.getValue().toString().equals(s2.getValue().toString());
    }

    public static ApplicationStatus getApplicationStatus(ReportNode reportNode) {
        if (reportNode.getChildren() != null && !reportNode.getChildren().isEmpty()) {
            return reportNode.getChildren().stream().map(NetworkModificationApplicator::getApplicationStatus)
                    .reduce(ApplicationStatus::max)
                    .orElse(ApplicationStatus.ALL_OK);
        }

        TypedValue severity = reportNode.getValues().get(ReportConstants.SEVERITY_KEY);
        if (severity == null || areSeveritiesEquals(severity, TypedValue.TRACE_SEVERITY) || areSeveritiesEquals(severity, TypedValue.DEBUG_SEVERITY) || areSeveritiesEquals(severity, TypedValue.INFO_SEVERITY)) {
            return ApplicationStatus.ALL_OK;
        } else if (areSeveritiesEquals(severity, TypedValue.WARN_SEVERITY)) {
            return ApplicationStatus.WITH_WARNINGS;
        } else if (areSeveritiesEquals(severity, TypedValue.ERROR_SEVERITY)) {
            return ApplicationStatus.WITH_ERRORS;
        } else {
            throw new IllegalArgumentException(String.format("Report severity '%s' unknown !", severity.getValue()));
        }
    }

    @PreDestroy
    public void shutdown() {
        applicationExecutor.shutdown();
    }
}
