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
import org.gridsuite.modification.server.dto.CompositeModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.gridsuite.modification.server.NetworkModificationException.Type.COMPOSITE_MODIFICATION_ERROR;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class CompositeModification extends AbstractModification {

    private final CompositeModificationInfos modificationInfos;

    private static final String COMPOSITE_MODIFICATION_REPORT_KEY_PREFIX = "composite";

    private static final Logger LOGGER = LoggerFactory.getLogger(CompositeModification.class);

    public CompositeModification(CompositeModificationInfos compositeModificationInfos) {
        this.modificationInfos = compositeModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(COMPOSITE_MODIFICATION_ERROR, "No composite modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        int applicationFailuresCount = 0;
        int modificationsCount = modificationInfos.getCompositeModificationsList().size();

        for (var modificationCompositeInfos : modificationInfos.getCompositeModificationsList()) {
            try {
                AbstractModification modification = modificationCompositeInfos.toModification();
                modification.check(network);
                modification.apply(network);
            } catch (PowsyblException e) {
                applicationFailuresCount++;
                subReportNode.newReportNode()
                        .withMessageTemplate(modificationCompositeInfos.getType().name() + applicationFailuresCount, "${message}")
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
                LOGGER.warn(e.getMessage());
            }
        }

        String typeName = modificationInfos.getType().name();
        String baseTemplate = COMPOSITE_MODIFICATION_REPORT_KEY_PREFIX + typeName;
        String defaultMessage = modificationInfos.getType().toString();
        if (applicationFailuresCount == modificationsCount) {
            subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplate + "Error", "Composite modification")
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (applicationFailuresCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplate + "Warning", "Composite modification")
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
        } else {
            subReportNode.newReportNode()
                    .withMessageTemplate(baseTemplate, "Composite modification")
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }
}
