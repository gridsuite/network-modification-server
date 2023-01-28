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
import com.powsybl.network.store.client.NetworkStoreService;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.service.ReportService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationApplicator {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationApplicator.class);

    private enum ApplicationMode {
        UNITARY,
        MULTIPLE
    }

    private static final String NETWORK_MODIFICATION_TYPE_REPORT = "NetworkModification";

    private final NetworkStoreService networkStoreService;

    private final EquipmentInfosService equipmentInfosService;

    private final ReportService reportService;

    private final ApplicationContext context;

    public NetworkModificationApplicator(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                         ReportService reportService, ApplicationContext context) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.reportService = reportService;
        this.context = context;
    }

    public List<ModificationInfos> applyModification(ModificationInfos modificationInfos, NetworkInfos networkInfos, ReportInfos reportInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService);
        apply(modificationInfos, listener, reportInfos, ApplicationMode.UNITARY);
        return listener.flushNetworkModifications();
    }

    public List<ModificationInfos> applyModifications(List<ModificationInfos> modificationInfosList, NetworkInfos networkInfos, ReportInfos reportInfos) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService);
        modificationInfosList.forEach(m -> apply(m, listener, reportInfos, ApplicationMode.MULTIPLE));
        return listener.flushNetworkModifications();
    }

    public List<ModificationInfos> applyModifications(List<Pair<String, List<ModificationInfos>>> modificationInfosGroups, NetworkInfos networkInfos, UUID reportUuid) {
        NetworkStoreListener listener = NetworkStoreListener.create(networkInfos.getNetwork(), networkInfos.getNetworkUuuid(), networkStoreService, equipmentInfosService);
        modificationInfosGroups.forEach(g -> g.getRight().forEach(m -> apply(m, listener, new ReportInfos(reportUuid, g.getLeft()), ApplicationMode.MULTIPLE)));
        return listener.flushNetworkModifications();
    }

    private void apply(ModificationInfos modificationInfos, NetworkStoreListener listener, ReportInfos reportInfos, ApplicationMode mode) {
        String rootReporterId = reportInfos.getReporterId() + "@" + NETWORK_MODIFICATION_TYPE_REPORT;
        ReporterModel reporter = new ReporterModel(rootReporterId, rootReporterId);
        Reporter subReporter = modificationInfos.createSubReporter(reporter);

        try {
            modificationInfos.toModification().apply(listener.getNetwork(), subReporter, context);
            listener.addNetworkDamage(modificationInfos);
        } catch (Exception e) {
            NetworkModificationException networkModificationException = handleException(modificationInfos.getErrorType(), subReporter, e);
            if (mode == ApplicationMode.UNITARY) {
                throw networkModificationException;
            }
        } finally {
            reportService.sendReport(reportInfos.getReportUuid(), reporter); // TODO : Group report sends ?
        }
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
}
