/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABULAR_MODIFICATION_ERROR;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class TabularModification extends AbstractModification {

    private static final Logger LOGGER = LoggerFactory.getLogger(TabularModification.class);

    private static final String TABULAR_MODIFICATION_REPORT_KEY_PREFIX = "tabular";

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
    public void apply(Network network, ReportNode subReportNode) {
        int applicationFailuresCount = 0;
        for (var modifInfos : modificationInfos.getModifications()) {
            try {
                AbstractModification modification = modifInfos.toModification();
                modification.check(network);
                if (modifInfos instanceof ShuntCompensatorModificationInfos shuntModification) {
                    checkShuntCompensatorModification(network, shuntModification, subReportNode);
                }
                modification.apply(network);
            } catch (PowsyblException e) {
                applicationFailuresCount++;
                subReportNode.newReportNode()
                        .withMessageTemplate(modifInfos.getType().name() + applicationFailuresCount, "${message}")
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
                LOGGER.warn(e.getMessage());
            }
        }
        String defaultMessage = switch (modificationInfos.getModificationType()) {
            case GENERATOR_MODIFICATION -> "generators";
            case LOAD_MODIFICATION -> "loads";
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> "two windings transformers";
            case BATTERY_MODIFICATION -> "batteries";
            case VOLTAGE_LEVEL_MODIFICATION -> "voltage levels";
            case SHUNT_COMPENSATOR_MODIFICATION -> "shunt compensators";
            case LINE_MODIFICATION -> "lines";
            case SUBSTATION_MODIFICATION -> "substations";
            default -> "equipments of unknown type";
        } + " have been modified";

        if (modificationInfos.getModifications().size() == applicationFailuresCount) {
            subReportNode.newReportNode()
                    .withMessageTemplate(TABULAR_MODIFICATION_REPORT_KEY_PREFIX + modificationInfos.getModificationType().name() + "Error", "Tabular modification: No ${defaultMessage}")
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (applicationFailuresCount > 0) {
            subReportNode.newReportNode()
                    .withMessageTemplate(TABULAR_MODIFICATION_REPORT_KEY_PREFIX + modificationInfos.getModificationType().name() + "Warning", "Tabular modification: ${modificationsCount} ${defaultMessage} and ${failuresCount} have not been modified")
                    .withUntypedValue("modificationsCount", modificationInfos.getModifications().size() - applicationFailuresCount)
                    .withUntypedValue("failuresCount", applicationFailuresCount)
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .add();
        } else {
            subReportNode.newReportNode()
                    .withMessageTemplate(TABULAR_MODIFICATION_REPORT_KEY_PREFIX + modificationInfos.getModificationType().name(), "Tabular modification: ${modificationsCount} ${defaultMessage}")
                    .withUntypedValue("modificationsCount", modificationInfos.getModifications().size())
                    .withUntypedValue("defaultMessage", defaultMessage)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    public void checkShuntCompensatorModification(
            Network network,
            ShuntCompensatorModificationInfos shuntCompensatorModificationInfos,
            ReportNode subReportNode
    ) {
        var shuntCompensator = network.getShuntCompensator(shuntCompensatorModificationInfos.getEquipmentId());
        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.NON_LINEAR) {
            subReportNode.newReportNode()
                    .withMessageTemplate(shuntCompensator.getId(), "Tabular modification: It is currently not possible to modify non-linear shunt compensator with id ${equipmentId}")
                    .withUntypedValue("equipmentId", shuntCompensator.getId())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
        } else if (shuntCompensatorModificationInfos.getMaxSusceptance() != null) {
            if (shuntCompensatorModificationInfos.getShuntCompensatorType() != null && shuntCompensatorModificationInfos.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate(shuntCompensator.getId(), "Tabular modification: Input for maximum susceptance has been ignored since it is not possible to simultaneously update type, maximum reactive power and maximum susceptance for shunt compensator with id ${equipmentId}")
                        .withUntypedValue("equipmentId", shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorModificationInfos.getShuntCompensatorType() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate(shuntCompensator.getId(), "Tabular modification: Input for maximum susceptance has been ignored since it is not possible to simultaneously update type and maximum susceptance for shunt compensator with id ${equipmentId}")
                        .withUntypedValue("equipmentId", shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            } else if (shuntCompensatorModificationInfos.getMaxQAtNominalV() != null) {
                subReportNode.newReportNode()
                        .withMessageTemplate(shuntCompensator.getId(), "Tabular modification: Input for maximum susceptance has been ignored since it is not possible to simultaneously update maximum reactive power and maximum susceptance for shunt compensator with id ${equipmentId}")
                        .withUntypedValue("equipmentId", shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .add();
            }
        }
    }
}
