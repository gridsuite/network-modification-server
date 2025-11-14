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
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.server.dto.ModificationApplicationGroup;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.function.Supplier;

import static org.gridsuite.modification.server.modifications.NetworkUtils.switchOnExistingVariant;
import static org.gridsuite.modification.server.modifications.NetworkUtils.switchOnNewVariant;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationApplicator {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationApplicator.class);

    private final ReportService reportService;

    private final FilterService filterService;

    private final LoadFlowService loadFlowService;

    private final LargeNetworkModificationExecutionService largeNetworkModificationExecutionService;

    private final NetworkModificationObserver networkModificationObserver;

    private final NamingStrategy namingStrategy;

    private final ModificationNetworkService modificationNetworkService;

    public NetworkModificationApplicator(ReportService reportService,
                                         FilterService filterService,
                                         LoadFlowService loadFlowService,
                                         NetworkModificationObserver networkModificationObserver,
                                         LargeNetworkModificationExecutionService largeNetworkModificationExecutionService,
                                         NamingStrategy namingStrategy, ModificationNetworkService modificationNetworkService) {
        this.reportService = reportService;
        this.filterService = filterService;
        this.loadFlowService = loadFlowService;
        this.networkModificationObserver = networkModificationObserver;
        this.largeNetworkModificationExecutionService = largeNetworkModificationExecutionService;
        this.namingStrategy = namingStrategy;
        this.modificationNetworkService = modificationNetworkService;
    }

    public CompletableFuture<NetworkModificationResult> applyModificationGroupIncremental(
        ModificationApplicationGroup modificationApplicationGroup,
        UUID networkUuid,
        String variantId
    ) {
        if (!modificationApplicationGroup.modifications().isEmpty()) {
            PreloadingStrategy preloadingStrategy = getPreloadingStrategy(modificationApplicationGroup.modifications());
            ModificationNetwork modificationNetwork = modificationNetworkService.getNetwork(
                networkUuid,
                preloadingStrategy
            );
            if (switchOnExistingVariant(modificationNetwork.network(), variantId)) {
                return executeApplications(
                    () -> applyModificationGroups(List.of(modificationApplicationGroup), modificationNetwork),
                    preloadingStrategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW
                );
            }
        }
        // In case there are no modifications to apply or that the variant does not exist i.e. the node is not built
        return CompletableFuture.completedFuture(
            NetworkModificationResult.builder()
                .applicationStatus(ApplicationStatus.ALL_OK)
                .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
                .build()
        );
    }

    /* This method is used when building a variant
     * building a variant is limited to ${consumer.concurrency} (typically 2) concurrent builds thanks to rabbitmq queue.
     * Note : it is possible that the rabbitmq consumer threads here will be blocked by modifications applied directly in the other applyModificationGroupIncremental method
     * and no more builds can go through. If this causes problems we should put them in separate rabbitmq queues.
     */
    public NetworkModificationResult applyModificationGroups(
        List<ModificationApplicationGroup> modificationApplicationGroups,
        UUID networkUuid,
        String originVariantId,
        String destinationVariantId
    ) {
        var modifications = modificationApplicationGroups.stream()
            .map(ModificationApplicationGroup::modifications)
            .flatMap(Collection::stream)
            .toList();
        PreloadingStrategy preloadingStrategy = getPreloadingStrategy(modifications);
        ModificationNetwork modificationNetwork = modificationNetworkService.getNetwork(networkUuid, preloadingStrategy);
        switchOnNewVariant(modificationNetwork.network(), originVariantId, destinationVariantId);
        return executeApplications(
            () -> applyModificationGroups(modificationApplicationGroups, modificationNetwork),
            preloadingStrategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW
        ).join();
    }

    private NetworkModificationResult applyModificationGroups(
        List<ModificationApplicationGroup> modificationApplicationGroups,
        ModificationNetwork modificationNetwork
    ) {
        List<ApplicationStatus> statuses = modificationApplicationGroups.stream()
            .map(g -> applyModificationGroup(g, modificationNetwork))
            .toList();
        List<AbstractBaseImpact> networkImpacts = modificationNetwork.flush();
        return NetworkModificationResult.builder()
            .applicationStatus(statuses.stream().reduce(ApplicationStatus::max).orElse(ApplicationStatus.ALL_OK))
            .lastGroupApplicationStatus(Streams.findLast(statuses.stream()).orElse(ApplicationStatus.ALL_OK))
            .networkImpacts(networkImpacts)
            .build();
    }

    private ApplicationStatus applyModificationGroup(
        ModificationApplicationGroup modificationGroupInfos,
        ModificationNetwork modificationNetwork
    ) {
        ReportNode reportNode = createModificationApplicationGroupReportNode(modificationGroupInfos);
        ApplicationStatus groupApplicationStatus = modificationGroupInfos.modifications().stream()
            .filter(m -> m.getActivated() && !m.getStashed())
            .map(m -> {
                modificationNetwork.initModificationApplication(modificationGroupInfos.groupUuid(), m);
                return applyModification(m.toModificationInfos(), modificationNetwork, reportNode);
            })
            .reduce(ApplicationStatus::max)
            .orElse(ApplicationStatus.ALL_OK);
        if (modificationGroupInfos.reportInfos().getReportUuid() != null) {
            reportService.sendReport(modificationGroupInfos.reportInfos().getReportUuid(), reportNode);
        }
        return groupApplicationStatus;
    }

    private ApplicationStatus applyModification(
        ModificationInfos modificationInfos,
        ModificationNetwork modificationNetwork,
        ReportNode reportNode
    ) {
        ReportNode subReportNode = modificationInfos.createSubReportNode(reportNode);
        try {
            networkModificationObserver.observeApply(modificationInfos.getType(), () -> {
                AbstractModification modification = modificationInfos.toModification();
                modification.check(modificationNetwork.network()); // check input data but don't change the network
                modification.initApplicationContext(filterService, loadFlowService);
                modification.apply(modificationNetwork.network(), namingStrategy, subReportNode);
            });
        } catch (Exception e) {
            handleException(subReportNode, e);
        }
        return getApplicationStatus(reportNode);
    }

    /* For all memory intensive applications we want to use the same ExecutorService to globally limit the number
     * of concurrent large modifications in order to avoid out of memory issues.
     * We keep the possibility to applyModification small or medium modifications immediately.
     * And if in the future we also need to limit the memory consumption of medium modifications we can add more code here.
     * Note : we currently have 3 sizes of modifications :
     * small : preloadingStrategy = NONE
     * medium : preloadingStrategy = COLLECTION
     * large : preloadingStrategy = ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW
     */
    private CompletableFuture<NetworkModificationResult> executeApplications(Supplier<NetworkModificationResult> supplier, boolean isLarge) {
        if (isLarge) {
            return largeNetworkModificationExecutionService.supplyAsync(supplier);
        } else {
            return CompletableFuture.completedFuture(supplier.get());
        }
    }

    private void handleException(ReportNode subReportNode, Exception e) {
        boolean isApplicationException = PowsyblException.class.isAssignableFrom(e.getClass());
        if (!isApplicationException && LOGGER.isErrorEnabled()) {
            LOGGER.error(e.toString(), e);
        }
        String errorMessage = isApplicationException ? e.getMessage() : "Technical error: " + e;

        subReportNode.newReportNode()
                .withMessageTemplate(ERROR_MESSAGE_KEY)
                .withTypedValue("typedValue", 20, "type")
                .withUntypedValue("errorMessage", errorMessage)
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .add();
    }

    private ReportNode createModificationApplicationGroupReportNode(ModificationApplicationGroup modificationGroupInfos) {
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
        return reportNode;
    }

    private PreloadingStrategy getPreloadingStrategy(List<ModificationEntity> modificationApplicationGroup) {
        return modificationApplicationGroup.stream()
            .map(ModificationEntity::getType)
            .map(ModificationType::valueOf)
            .reduce(ModificationType::maxStrategy)
            .map(ModificationType::getStrategy)
            .orElse(PreloadingStrategy.NONE);
    }

    private static boolean areSeveritiesEquals(TypedValue s1, TypedValue s2) {
        return s1.getValue().toString().equals(s2.getValue().toString());
    }

    // public only for tests
    public static ApplicationStatus getApplicationStatus(ReportNode reportNode) {
        if (reportNode.getChildren() != null && !reportNode.getChildren().isEmpty()) {
            return reportNode.getChildren().stream().map(NetworkModificationApplicator::getApplicationStatus)
                    .reduce(ApplicationStatus::max)
                    .orElse(ApplicationStatus.ALL_OK);
        }

        TypedValue severity = reportNode.getValues().get(ReportConstants.SEVERITY_KEY);
        if (severity == null || areSeveritiesEquals(severity, TypedValue.TRACE_SEVERITY) || areSeveritiesEquals(severity, TypedValue.DEBUG_SEVERITY) || areSeveritiesEquals(severity, TypedValue.INFO_SEVERITY) || areSeveritiesEquals(severity, TypedValue.DETAIL_SEVERITY)) {
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
