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
import lombok.Getter;
import lombok.Setter;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.server.dto.ModificationApplicationGroup;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.LargeNetworkModificationExecutionService;
import org.gridsuite.modification.server.service.NetworkModificationObserver;
import org.gridsuite.modification.server.service.ReportService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationApplicator {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationApplicator.class);

    private final NetworkStoreService networkStoreService;

    private final EquipmentInfosService equipmentInfosService;

    private final ModificationApplicationInfosService applicationInfosService;

    private final ReportService reportService;

    @Getter private final FilterService filterService;

    private final LargeNetworkModificationExecutionService largeNetworkModificationExecutionService;

    private final NetworkModificationObserver networkModificationObserver;

    @Value("${impacts.collection-threshold:50}")
    @Setter // TODO REMOVE when VoltageInitReportTest will no longer use NetworkModificationApplicator
    private Integer collectionThreshold;

    public NetworkModificationApplicator(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                         ModificationApplicationInfosService applicationInfosService,
                                         ReportService reportService, FilterService filterService,
                                         NetworkModificationObserver networkModificationObserver,
                                         LargeNetworkModificationExecutionService largeNetworkModificationExecutionService) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.applicationInfosService = applicationInfosService;
        this.reportService = reportService;
        this.filterService = filterService;
        this.networkModificationObserver = networkModificationObserver;
        this.largeNetworkModificationExecutionService = largeNetworkModificationExecutionService;
    }

    /* This method is used for incremental modifications
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
    public NetworkModificationResult applyModifications(ModificationApplicationGroup modificationInfosGroup, NetworkInfos networkInfos) {
        PreloadingStrategy preloadingStrategy = modificationInfosGroup.modifications().stream()
            .map(ModificationEntity::getType)
            .map(ModificationType::valueOf)
            .reduce(ModificationType::maxStrategy)
            .map(ModificationType::getStrategy)
            .orElse(PreloadingStrategy.NONE);

        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService, applicationInfosService, collectionThreshold);
        ApplicationStatus groupApplicationStatus;
        if (preloadingStrategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW) {
            groupApplicationStatus = largeNetworkModificationExecutionService
                .supplyAsync(() -> apply(modificationInfosGroup, listener))
                .join();
        } else {
            groupApplicationStatus = apply(modificationInfosGroup, listener);
        }

        return flushModificationApplications(groupApplicationStatus, listener);
    }

    private NetworkModificationResult flushModificationApplications(ApplicationStatus groupApplicationStatus, NetworkStoreListener listener) {
        List<AbstractBaseImpact> networkImpacts = listener.flushModificationApplications();
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
    public NetworkModificationResult applyModifications(List<ModificationApplicationGroup> modificationInfosGroups, NetworkInfos networkInfos) {
        PreloadingStrategy preloadingStrategy = modificationInfosGroups.stream()
                .map(ModificationApplicationGroup::modifications)
                .flatMap(List::stream)
                .map(ModificationEntity::getType)
                .map(ModificationType::valueOf)
                .reduce(ModificationType::maxStrategy)
                .map(ModificationType::getStrategy)
                .orElse(PreloadingStrategy.NONE);

        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService, applicationInfosService, collectionThreshold);
        List<ApplicationStatus> groupsApplicationStatuses;
        if (preloadingStrategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW) {
            groupsApplicationStatuses = largeNetworkModificationExecutionService
                .supplyAsync(() -> apply(modificationInfosGroups, listener))
                .join();
        } else {
            groupsApplicationStatuses = apply(modificationInfosGroups, listener);
        }

        return flushModificationApplications(groupsApplicationStatuses, listener);
    }

    // This method is used when building a variant
    private List<ApplicationStatus> apply(List<ModificationApplicationGroup> modificationInfosGroups, NetworkStoreListener listener) {
        return modificationInfosGroups.stream()
            .map(g -> apply(g, listener))
            .toList();
    }

    // This method is used when building a variant
    private NetworkModificationResult flushModificationApplications(List<ApplicationStatus> groupsApplicationStatuses, NetworkStoreListener listener) {
        List<AbstractBaseImpact> networkImpacts = listener.flushModificationApplications();
        return NetworkModificationResult.builder()
            .applicationStatus(groupsApplicationStatuses.stream().reduce(ApplicationStatus::max).orElse(ApplicationStatus.ALL_OK))
            .lastGroupApplicationStatus(Streams.findLast(groupsApplicationStatuses.stream()).orElse(ApplicationStatus.ALL_OK))
            .networkImpacts(networkImpacts)
            .build();
    }

    private ApplicationStatus apply(ModificationApplicationGroup modificationGroupInfos, NetworkStoreListener listener) {
        ReportNode reportNode;
        if (modificationGroupInfos.reportInfos().getNodeUuid() != null) {
            UUID reporterId = modificationGroupInfos.reportInfos().getNodeUuid();
            reportNode = ReportNode.newRootReportNode()
                    .withAllResourceBundlesFromClasspath()
                    .withMessageTemplate("network.modification.server.nodeUuid")
                    .withUntypedValue("nodeUuid", reporterId.toString())
                    .build();
        } else {
            reportNode = ReportNode.NO_OP;
        }
        ApplicationStatus groupApplicationStatus = modificationGroupInfos.modifications().stream()
                .filter(ModificationEntity::getActivated)
                .map(m -> {
                    listener.initModificationApplication(modificationGroupInfos.groupUuid(), m);
                    return apply(m.toModificationInfos(), listener.getNetwork(), reportNode);
                })
                .reduce(ApplicationStatus::max)
                .orElse(ApplicationStatus.ALL_OK);
        if (modificationGroupInfos.reportInfos().getReportUuid() != null) {
            reportService.sendReport(modificationGroupInfos.reportInfos().getReportUuid(), reportNode);
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
        modification.initApplicationContext(this.filterService);

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
}
