/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.TabularCreationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABULAR_CREATION_ERROR;
import static org.gridsuite.modification.server.modifications.ConstantUtil.TABULAR_MODIFICATION_REPORT_KEY_PREFIX;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class TabularCreation extends AbstractModification {

    private static final Logger LOGGER = LoggerFactory.getLogger(TabularCreation.class);

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
    public void apply(Network network, ReportNode subReportNode) {
        int applicationFailuresCount = 0;
        for (var creatInfos : creationInfos.getCreations()) {
            try {
                AbstractModification modification = creatInfos.toModification();
                modification.check(network);
                modification.apply(network);
            } catch (PowsyblException e) {
                applicationFailuresCount++;
                subReportNode.newReportNode()
                        .withMessageTemplate(creatInfos.getType().name() + applicationFailuresCount, "${message}")
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
                LOGGER.warn(e.getMessage());
            }
        }
        String defaultMessage = switch (creationInfos.getCreationType()) {
            case GENERATOR_CREATION -> "generators";
            default -> "equipments of unknown type";
        } + " have been created";

        if (creationInfos.getCreations().size() == applicationFailuresCount) {
            subReportNode.newReportNode()
                    .withMessageTemplate(TABULAR_MODIFICATION_REPORT_KEY_PREFIX + creationInfos.getCreationType().name() + "Error", "Tabular creation: No ${defaultMessage}")
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (applicationFailuresCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate(TABULAR_MODIFICATION_REPORT_KEY_PREFIX + creationInfos.getCreationType().name() + "Warning", "Tabular creation: ${creationsCount} ${defaultMessage} and ${failuresCount} have not been created")
                    .withUntypedValue("creationsCount", creationInfos.getCreations().size() - applicationFailuresCount)
                    .withUntypedValue("failuresCount", applicationFailuresCount)
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
        } else {
            subReportNode.newReportNode()
                    .withMessageTemplate(TABULAR_MODIFICATION_REPORT_KEY_PREFIX + creationInfos.getCreationType().name(), "Tabular creation: ${creationsCount} ${defaultMessage}")
                    .withUntypedValue("creationsCount", creationInfos.getCreations().size())
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }
}
