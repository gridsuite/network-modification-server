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

    private final ExecutorService executorService;

    private final ExecutorService applicationExecutor;

    @Value("${impacts.collection-threshold:50}")
    @Setter // TODO REMOVE when VoltageInitReportTest will no longer use NetworkModificationApplicator
    private Integer collectionThreshold;

    public NetworkModificationApplicator(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                         ReportService reportService, FilterService filterService, @Value("${max-concurrent-voltage-init}") int maxConcurrentNadGenerations,
                                         @Value("${max-concurrent-applications}") int maxConcurrentApplications) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.reportService = reportService;
        this.filterService = filterService;
        this.executorService = Executors.newFixedThreadPool(maxConcurrentNadGenerations);
        this.applicationExecutor = Executors.newFixedThreadPool(maxConcurrentApplications);
    }

    /* This method is used when creating, inserting, moving or duplicating modifications
     * Since there is no queue for these operations and they can be memory consuming
     * so we limit the number of concurrent applications of these modifications to avoid memory issues
     */
    public NetworkModificationResult applyModifications(List<ModificationInfos> modificationInfosList, NetworkInfos networkInfos, ReportInfos reportInfos) {
        CompletableFuture<NetworkModificationResult> future = CompletableFuture.supplyAsync(() -> {
            NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService, collectionThreshold);
            ApplicationStatus groupApplicationStatus = apply(modificationInfosList, listener.getNetwork(), reportInfos);
            List<AbstractBaseImpact> networkImpacts = listener.flushNetworkModifications();
            return
                NetworkModificationResult.builder()
                    .applicationStatus(groupApplicationStatus)
                    .lastGroupApplicationStatus(groupApplicationStatus)
                    .networkImpacts(networkImpacts)
                    .build();
        }, applicationExecutor);
        return future.join();
    }

    /* This method is used when building a variant
     * building a variant is limited to 2 concurrent builds thanks to rabbitmq queue
     * but since the other operations (create, insert, move, duplicate) are not inserted in the same rabbitmq queue
     * we use the same ExecutorService to control the number of concurrent applications in order to avoid memory issues
     * Example: when building 2 variants, the user can insert one or more composite modification that will be applied immediately
     */
    public NetworkModificationResult applyModifications(List<Pair<String, List<ModificationInfos>>> modificationInfosGroups, NetworkInfos networkInfos, UUID reportUuid) {
        CompletableFuture<NetworkModificationResult> future = CompletableFuture.supplyAsync(() -> {
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
        }, applicationExecutor);
        return future.join();
    }

    private ApplicationStatus apply(List<ModificationInfos> modificationInfosList, Network network, ReportInfos reportInfos) {
        String rootReporterId = reportInfos.getReporterId() + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReportNode reportNode = ReportNode.newRootReportNode().withMessageTemplate(rootReporterId, rootReporterId).build();
        ApplicationStatus groupApplicationStatus = modificationInfosList.stream()
                .map(m -> {
                    // voltage init modifications are the most memory consuming modifications
                    // so we need to limit the number of concurrent applications to avoid memory issues
                    if (m.getType() == ModificationType.VOLTAGE_INIT_MODIFICATION) {
                        return CompletableFuture.supplyAsync(() -> apply(m, network, reportNode), executorService).join();
                    } else {
                        return apply(m, network, reportNode);
                    }
                })
                .reduce(ApplicationStatus::max)
                .orElse(ApplicationStatus.ALL_OK);
        reportService.sendReport(reportInfos.getReportUuid(), reportNode);
        return groupApplicationStatus;
    }

    private ApplicationStatus apply(ModificationInfos modificationInfos, Network network, ReportNode reportNode) {
        ReportNode subReportNode = modificationInfos.createSubReportNode(reportNode);
        try {
            apply(modificationInfos.toModification(), network, subReportNode);
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

        TypedValue severity = reportNode.getValues().get(ReportConstants.REPORT_SEVERITY_KEY);
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
        executorService.shutdown();
        applicationExecutor.shutdown();
    }
}
