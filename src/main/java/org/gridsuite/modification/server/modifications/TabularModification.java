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
        int applicationFailuresCount = 0;
        for (var modifInfos : modificationInfos.getModifications()) {
            if (modifInfos instanceof ShuntCompensatorModificationInfos shuntModification) {
                boolean isModifOK = checkShuntCompensatorModification(network, shuntModification, subReporter);
                if (!isModifOK) {
                    applicationFailuresCount++;
                }
            }
            try {
                AbstractModification modification = modifInfos.toModification();
                modification.check(network);
                modification.apply(network);
            } catch (PowsyblException e) {
                applicationFailuresCount++;
                subReporter.report(Report.builder()
                        .withKey(modifInfos.getType().name() + applicationFailuresCount)
                        .withDefaultMessage(e.getMessage())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
                LOGGER.warn(e.getMessage());
            }
        }
        String defaultMessage = " have been modified";
        defaultMessage = switch (modificationInfos.getModificationType()) {
            case "GENERATOR_MODIFICATION" -> "generators" + defaultMessage;
            case "LOAD_MODIFICATION" -> "loads" + defaultMessage;
            case "TWO_WINDINGS_TRANSFORMER_MODIFICATION" -> "two windings transformers" + defaultMessage;
            case "BATTERY_MODIFICATION" -> "batteries" + defaultMessage;
            case "VOLTAGE_LEVEL_MODIFICATION" -> "voltage levels" + defaultMessage;
            case "SHUNT_COMPENSATOR_MODIFICATION" -> "shunt compensator" + defaultMessage;
            case "LINE_MODIFICATION" -> "lines" + defaultMessage;
            default -> "equipments of unknown type" + defaultMessage;
        };

        if (modificationInfos.getModifications().size() == applicationFailuresCount) {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_MODIFICATION_REPORT_KEY + "Error")
                    .withDefaultMessage("Tabular modification: No " + defaultMessage)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
        } else if (applicationFailuresCount > 0) {
            subReporter.report(Report.builder()
                    .withKey(TABULAR_MODIFICATION_REPORT_KEY + "Warning")
                    .withDefaultMessage("Tabular modification: ${modificationsCount} " + defaultMessage + " and ${failuresCount} have not been modified")
                    .withValue("modificationsCount", modificationInfos.getModifications().size() - applicationFailuresCount)
                    .withValue("failuresCount", applicationFailuresCount)
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

    private boolean checkShuntCompensatorModification(
            Network network,
            ShuntCompensatorModificationInfos shuntCompensatorModificationInfos,
            Reporter subReporter
    ) {
        var shuntCompensator = network.getShuntCompensator(shuntCompensatorModificationInfos.getEquipmentId());
        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.NON_LINEAR) {
            subReporter.report(Report.builder()
                    .withKey(shuntCompensator.getId())
                    .withDefaultMessage("Tabular modification: It is currently not possible to modify non-linear shunt compensator with id " + shuntCompensator.getId())
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
            return false;
        }
        if (shuntCompensatorModificationInfos.getMaxSusceptance() != null) {
            if (shuntCompensatorModificationInfos.getShuntCompensatorType() != null && shuntCompensatorModificationInfos.getMaxQAtNominalV() != null) {
                subReporter.report(Report.builder()
                        .withKey(shuntCompensator.getId())
                        .withDefaultMessage("Tabular modification: Input for maximum susceptance has been ignored since it is not possible to simultaneously update type, maximum reactive power and maximum susceptance for shunt compensator with id "
                                + shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (shuntCompensatorModificationInfos.getShuntCompensatorType() != null) {
                subReporter.report(Report.builder()
                        .withKey(shuntCompensator.getId())
                        .withDefaultMessage("Tabular modification: Input for maximum susceptance has been ignored since it is not possible to simultaneously update type and maximum susceptance for shunt compensator with id "
                                + shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else if (shuntCompensatorModificationInfos.getMaxQAtNominalV() != null) {
                subReporter.report(Report.builder()
                        .withKey(shuntCompensator.getId())
                        .withDefaultMessage("Tabular modification: Input for maximum susceptance has been ignored since it is not possible to simultaneously update maximum reactive power and maximum susceptance for shunt compensator with id "
                                + shuntCompensator.getId())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }
        }
        return true;
    }
}
