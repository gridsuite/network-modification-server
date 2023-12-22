/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.atomic.AtomicInteger;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABULAR_MODIFICATION_ERROR;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class TabularModification extends AbstractModification {

    private static final Logger LOGGER = LoggerFactory.getLogger(TabularModification.class);

    private static final String TABULAR_MODIFICATION_REPORT_KEY = "tabularModification";

    private final TabularModificationInfos modificationInfos;

    public TabularModification(TabularModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(TABULAR_MODIFICATION_ERROR, "No tabular modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        AtomicInteger applicationFailuresCount = new AtomicInteger(0);
        modificationInfos.getModifications().forEach(modifInfos -> {
            try {
                AbstractModification modification = modifInfos.toModification();
                modification.check(network);
                modification.apply(network);
            } catch (PowsyblException e) {
                applicationFailuresCount.incrementAndGet();
                subReporter.report(Report.builder()
                        .withKey(modifInfos.getType().name() + applicationFailuresCount.get())
                        .withDefaultMessage(e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
                LOGGER.warn(e.getMessage());
            }
        });
        String defaultMessage = " have been modified";
        switch (modificationInfos.getModificationType()) {
            case "GENERATOR_MODIFICATION":
                defaultMessage = "generators" + defaultMessage;
                break;
            case "LOAD_MODIFICATION":
                defaultMessage = "loads" + defaultMessage;
                break;
            case "TWO_WINDINGS_TRANSFORMER_MODIFICATION":
                defaultMessage = "two windings transformers" + defaultMessage;
                break;
            case "BATTERY_MODIFICATION":
                defaultMessage = "batteries" + defaultMessage;
                break;
            case "VOLTAGE_LEVEL_MODIFICATION":
                defaultMessage = "voltage levels" + defaultMessage;
                break;
            default:
                defaultMessage = "equipments of unknown type" + defaultMessage;
                break;
        }

        if (modificationInfos.getModifications().size() == applicationFailuresCount.get()) {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_MODIFICATION_REPORT_KEY + "Error")
                    .withDefaultMessage("Tabular modification: No " + defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
        } else if (applicationFailuresCount.get() > 0) {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_MODIFICATION_REPORT_KEY + "Warning")
                    .withDefaultMessage("Tabular modification: ${modificationsCount} " + defaultMessage + " and ${failuresCount} have not been modified")
                    .withValue("modificationsCount", modificationInfos.getModifications().size() - applicationFailuresCount.get())
                    .withValue("failuresCount", applicationFailuresCount.get())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_MODIFICATION_REPORT_KEY)
                    .withDefaultMessage("Tabular modification: ${modificationsCount} " + defaultMessage)
                    .withValue("modificationsCount", modificationInfos.getModifications().size())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }
}
