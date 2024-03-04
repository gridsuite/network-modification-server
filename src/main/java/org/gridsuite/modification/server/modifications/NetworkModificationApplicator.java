/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.google.common.collect.Streams;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.tuple.Pair;
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
import java.util.stream.Stream;

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

    @Value("${impacts.collection-threshold:50}")
    @Setter // TODO REMOVE when VoltageInitReportTest will no longer use NetworkModificationApplicator
    private Integer collectionThreshold;

    public NetworkModificationApplicator(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                         ReportService reportService, FilterService filterService) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.reportService = reportService;
        this.filterService = filterService;
    }

    public NetworkModificationResult applyModifications(List<ModificationInfos> modificationInfosList, NetworkInfos networkInfos, ReportInfos reportInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService, collectionThreshold);
        ApplicationStatus groupApplicationStatus = apply(modificationInfosList, listener.getNetwork(), reportInfos);
        List<AbstractBaseImpact> networkImpacts = listener.flushNetworkModifications();
        return
            NetworkModificationResult.builder()
                .applicationStatus(groupApplicationStatus)
                .lastGroupApplicationStatus(groupApplicationStatus)
                .networkImpacts(networkImpacts)
                .build();
    }

    public NetworkModificationResult applyModifications(List<Pair<String, List<ModificationInfos>>> modificationInfosGroups, NetworkInfos networkInfos, UUID reportUuid) {
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
        String rootReporterId = reportInfos.getReporterId() + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        ApplicationStatus groupApplicationStatus = modificationInfosList.stream()
                .map(m -> apply(m, network, reporter))
                .reduce(ApplicationStatus::max)
                .orElse(ApplicationStatus.ALL_OK);
        reportService.sendReport(reportInfos.getReportUuid(), reporter);
        return groupApplicationStatus;
    }

    private ApplicationStatus apply(ModificationInfos modificationInfos, Network network, ReporterModel reporter) {
        Reporter subReporter = modificationInfos.createSubReporter(reporter);
        try {
            apply(modificationInfos.toModification(), network, subReporter);
        } catch (Exception e) {
            handleException(modificationInfos.getErrorType(), subReporter, e);
        }
        return getApplicationStatus(reporter);
    }

    private void apply(AbstractModification modification, Network network, Reporter subReporter) {
        // check input data but don't change the network
        modification.check(network);

        // init application context
        modification.initApplicationContext(this);

        // apply all changes on the network
        modification.apply(network, subReporter);
    }

    private void handleException(NetworkModificationException.Type typeIfError, Reporter subReporter, Exception e) {
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
    }

    public static ApplicationStatus getApplicationStatus(Report report) {
        TypedValue severity = report.getValues().get(Report.REPORT_SEVERITY_KEY);
        if (severity == null || severity == TypedValue.TRACE_SEVERITY || severity == TypedValue.DEBUG_SEVERITY || severity == TypedValue.INFO_SEVERITY) {
            return ApplicationStatus.ALL_OK;
        } else if (severity == TypedValue.WARN_SEVERITY) {
            return ApplicationStatus.WITH_WARNINGS;
        } else if (severity == TypedValue.ERROR_SEVERITY) {
            return ApplicationStatus.WITH_ERRORS;
        } else {
            throw new IllegalArgumentException(String.format("Report severity '%s' unknown !", severity.getValue()));
        }
    }

    public static ApplicationStatus getApplicationStatus(ReporterModel reporter) {
        return Stream.concat(
                        reporter.getReports().stream().map(NetworkModificationApplicator::getApplicationStatus),
                        reporter.getSubReporters().stream().map(NetworkModificationApplicator::getApplicationStatus)
                )
                .reduce(ApplicationStatus::max)
                .orElse(ApplicationStatus.ALL_OK);
    }
}
