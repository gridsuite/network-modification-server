/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.dto.TabularCreationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABULAR_CREATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class TabularCreation extends AbstractModification {

    private static final Logger LOGGER = LoggerFactory.getLogger(TabularCreation.class);

    private static final String TABULAR_CREATION_REPORT_KEY_PREFIX = "tabular";

    private final TabularCreationInfos creationInfos;

    public TabularCreation(TabularCreationInfos creationInfos) {
        this.creationInfos = creationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (creationInfos == null) {
            throw new NetworkModificationException(TABULAR_CREATION_ERROR, "No tabular creation to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        int applicationFailuresCount = 0;
        for (var creatInfos : creationInfos.getCreations()) {
            try {
                AbstractModification modification = creatInfos.toModification();
                modification.check(network);
                modification.apply(network);
            } catch (PowsyblException e) {
                applicationFailuresCount++;
                subReporter.report(Report.builder()
                        .withKey(creatInfos.getType().name() + applicationFailuresCount)
                        .withDefaultMessage("${message}")
                        .withValue("message", e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
                LOGGER.warn(e.getMessage());
            }
        }
        String defaultMessage = switch (creationInfos.getCreationType()) {
            case GENERATOR_CREATION -> "generators";
            default -> "equipments of unknown type";
        } + " have been created";

        if (creationInfos.getCreations().size() == applicationFailuresCount) {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_CREATION_REPORT_KEY_PREFIX + creationInfos.getCreationType().name() + "Error")
                    .withDefaultMessage("Tabular creation: No ${defaultMessage}")
                    .withValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
        } else if (applicationFailuresCount > 0) {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_CREATION_REPORT_KEY_PREFIX + creationInfos.getCreationType().name() + "Warning")
                    .withDefaultMessage("Tabular creation: ${creationsCount} ${defaultMessage} and ${failuresCount} have not been created")
                    .withValue("creationsCount", creationInfos.getCreations().size() - applicationFailuresCount)
                    .withValue("failuresCount", applicationFailuresCount)
                    .withValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_CREATION_REPORT_KEY_PREFIX + creationInfos.getCreationType().name())
                    .withDefaultMessage("Tabular creation: ${creationsCount} ${defaultMessage}")
                    .withValue("creationsCount", creationInfos.getCreations().size())
                    .withValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }
}
