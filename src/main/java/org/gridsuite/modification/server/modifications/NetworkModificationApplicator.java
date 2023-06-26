/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.impacts.BaseImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.service.ReportService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationApplicator {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationApplicator.class);

    private static final String NETWORK_MODIFICATION_TYPE_REPORT = "NetworkModification";

    private final NetworkStoreService networkStoreService;

    private final EquipmentInfosService equipmentInfosService;

    private final ReportService reportService;

    private final ApplicationContext context;

    private NetworkModificationResult.ApplicationStatus applicationStatus = NetworkModificationResult.ApplicationStatus.ALL_OK;

    public NetworkModificationApplicator(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                         ReportService reportService, ApplicationContext context) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.reportService = reportService;
        this.context = context;
    }

    public NetworkModificationResult applyModification(ModificationInfos modificationInfos, NetworkInfos networkInfos, ReportInfos reportInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService);
        apply(modificationInfos, listener, reportInfos);
        // here get impacts from the modification itself ?
        Set<BaseImpact> networkImpacts = apply(modificationInfos, listener, reportInfos);
        // Then append impacts from listerner
        networkImpacts.addAll(listener.flushNetworkModifications());
        // Bonus: Add a method wich cleans impacts
        // ex1: collection LOAD impact remove simpleElementImpact on a single Load)
        // ex2: if nb of simpleElementImpact of a type > N then replace by Collection impact etc...
        // Then build the NetworkModificationResult here
        return
            NetworkModificationResult.builder()
                .applicationStatus(applicationStatus)
                .networkImpacts(new ArrayList<>(networkImpacts))
                .build();
    }

    public NetworkModificationResult applyModifications(List<ModificationInfos> modificationInfosList, NetworkInfos networkInfos, ReportInfos reportInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService);
        modificationInfosList.forEach(m -> apply(m, listener, reportInfos));
        Set<SimpleElementImpact> networkImpacts = listener.flushNetworkModifications();

        return
            NetworkModificationResult.builder()
                .applicationStatus(applicationStatus)
                .networkImpacts(new ArrayList<>(networkImpacts))
                .build();
    }

    public NetworkModificationResult applyModifications(List<Pair<String, List<ModificationInfos>>> modificationInfosGroups, NetworkInfos networkInfos, UUID reportUuid) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService);
        modificationInfosGroups.forEach(g -> g.getRight().forEach(m -> apply(m, listener, new ReportInfos(reportUuid, g.getLeft()))));
        Set<SimpleElementImpact> networkImpacts = listener.flushNetworkModifications();

        return
            NetworkModificationResult.builder()
                .applicationStatus(applicationStatus)
                .networkImpacts(new ArrayList<>(networkImpacts))
                .build();
    }

    private Set<BaseImpact> apply(ModificationInfos modificationInfos, NetworkStoreListener listener, ReportInfos reportInfos) {
        String rootReporterId = reportInfos.getReporterId() + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = modificationInfos.createSubReporter(reporter);
        try {
            return apply(modificationInfos.toModification(), listener.getNetwork(), subReporter);
        } catch (Exception e) {
            handleException(modificationInfos.getErrorType(), subReporter, e);
        } finally {
            setApplicationStatus(getApplicationStatus(reporter));
            reportService.sendReport(reportInfos.getReportUuid(), reporter); // TODO : Group report sends ?
        }
        return Set.of();
    }

    @SuppressWarnings("squid:S1181")
    private Set<BaseImpact> apply(AbstractModification modification, Network network, Reporter subReporter) {
        try {
            // check input data but don't change the network
            modification.check(network);
            // apply all changes on the network
            return modification.apply(network, subReporter, context);
        } catch (Error e) {
            // TODO remove this catch with powsybl 5.2.0
            // Powsybl can raise Error
            // Ex: java.lang.AssertionError: The voltage level 'vlId' cannot be removed because of a remaining LINE
            throw new PowsyblException(e);
        }
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

    private void setApplicationStatus(NetworkModificationResult.ApplicationStatus applicationStatus) {
        this.applicationStatus = this.applicationStatus.max(applicationStatus);
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
