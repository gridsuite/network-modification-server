/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.SHUNT_COMPENSATOR_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_SHUNT_COMPENSATOR_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.insertReportNode;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class ShuntCompensatorModification extends AbstractModification {
    private static final String SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE = "Switched-on Q at nominal voltage";

    private final ShuntCompensatorModificationInfos modificationInfos;

    public ShuntCompensatorModification(ShuntCompensatorModificationInfos shuntCompensatorModificationInfos) {
        this.modificationInfos = shuntCompensatorModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationInfos.getEquipmentId());
        if (shuntCompensator == null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_NOT_FOUND,
                    String.format("Shunt compensator %s does not exist in network", modificationInfos.getEquipmentId()));
        }

        int maximumSectionCount = modificationInfos.getMaximumSectionCount() != null
                ? modificationInfos.getMaximumSectionCount().getValue()
                : shuntCompensator.getMaximumSectionCount();

        int sectionCount = modificationInfos.getSectionCount() != null
                ? modificationInfos.getSectionCount().getValue()
                : shuntCompensator.getSectionCount();

        if (modificationInfos.getMaximumSectionCount() != null && modificationInfos.getMaximumSectionCount().getValue() < 1) {
            throw new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR, "Maximum section count should be greater or equal to 1");
        }

        if (sectionCount < 0 || maximumSectionCount < 1 || sectionCount > maximumSectionCount) {
            throw new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR, String.format("Section count should be between 0 and Maximum section count (%d), actual : %d", maximumSectionCount, sectionCount));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationInfos.getEquipmentId());
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();

        subReportNode.newReportNode()
                .withMessageTemplate("shuntCompensatorModification", "Shunt Compensator with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        ModificationUtils.getInstance().applyElementaryModifications(shuntCompensator::setName, () -> shuntCompensator.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");

        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.LINEAR) {
            applyModificationOnLinearModel(subReportNode, shuntCompensator, voltageLevel);
        }

        ModificationUtils.getInstance().modifyInjectionConnection(modificationInfos, shuntCompensator);
        PropertiesUtils.applyProperties(shuntCompensator, subReportNode, modificationInfos.getProperties());

    }

    private void modifyMaximumSectionCount(List<ReportNode> reports, ShuntCompensator shuntCompensator, ShuntCompensatorLinearModel model) {
        if (modificationInfos.getMaximumSectionCount() != null) {
            var maximumSectionCount = modificationInfos.getMaximumSectionCount().getValue();
            if (modificationInfos.getMaxSusceptance() == null && modificationInfos.getMaxQAtNominalV() == null) {
                model.setBPerSection(model.getBPerSection() * shuntCompensator.getMaximumSectionCount() / maximumSectionCount);
            }
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensator.getMaximumSectionCount(), maximumSectionCount, "Maximum section count"));
            model.setMaximumSectionCount(maximumSectionCount);
        }
    }

    private void modifySectionCount(List<ReportNode> reports, ShuntCompensator shuntCompensator) {
        if (modificationInfos.getSectionCount() != null) {
            var newSectionCount = modificationInfos.getSectionCount().getValue();
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensator.getSectionCount(), newSectionCount, "Section count"));
            shuntCompensator.setSectionCount(newSectionCount);
        }
    }

    private void applyModificationOnLinearModel(ReportNode subReportNode, ShuntCompensator shuntCompensator, VoltageLevel voltageLevel) {
        List<ReportNode> reports = new ArrayList<>();
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        var shuntCompensatorType = model.getBPerSection() > 0 ? ShuntCompensatorType.CAPACITOR : ShuntCompensatorType.REACTOR;
        double oldSusceptancePerSection = model.getBPerSection();
        double oldQAtNominalV = Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * oldSusceptancePerSection);
        double oldMaxSusceptance = oldSusceptancePerSection * shuntCompensator.getMaximumSectionCount();
        double oldMaxQAtNominalV = oldQAtNominalV * shuntCompensator.getMaximumSectionCount();
        double oldSwitchedOnSusceptance = oldSusceptancePerSection * shuntCompensator.getSectionCount();
        double oldSwitchedOnQAtNominalV = oldQAtNominalV * shuntCompensator.getSectionCount();

        if (modificationInfos.getShuntCompensatorType() != null || modificationInfos.getMaxQAtNominalV() != null) {
            modificationInfos.setMaxSusceptance(null);
        }

        // due to cross validation between maximum section count and section count, we need to modify section count first
        // when maximum section count old value is greater than the new one
        if (modificationInfos.getMaximumSectionCount() != null && modificationInfos.getMaximumSectionCount().getValue() < shuntCompensator.getMaximumSectionCount()) {
            modifySectionCount(reports, shuntCompensator);
            modifyMaximumSectionCount(reports, shuntCompensator, model);
        } else {
            modifyMaximumSectionCount(reports, shuntCompensator, model);
            modifySectionCount(reports, shuntCompensator);
        }

        int maximumSectionCount = modificationInfos.getMaximumSectionCount() != null ? modificationInfos.getMaximumSectionCount().getValue() : shuntCompensator.getMaximumSectionCount();
        int sectionCount = modificationInfos.getSectionCount() != null ? modificationInfos.getSectionCount().getValue() : shuntCompensator.getSectionCount();

        if (modificationInfos.getShuntCompensatorType() != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensatorType, modificationInfos.getShuntCompensatorType().getValue(), "Type"));
            shuntCompensatorType = modificationInfos.getShuntCompensatorType().getValue();
            if (modificationInfos.getMaxQAtNominalV() == null) {
                // we retrieve the absolute value of susceptance per section, then we determine the sign using the type
                double bPerSectionAbsoluteValue = Math.abs(oldSusceptancePerSection);
                double newBPerSection = shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? bPerSectionAbsoluteValue : -bPerSectionAbsoluteValue;
                model.setBPerSection(newBPerSection);
            }
        }

        if (modificationInfos.getMaxQAtNominalV() != null) {
            if (modificationInfos.getMaxQAtNominalV().getValue() < 0) {
                throw new NetworkModificationException(NetworkModificationException.Type.MODIFY_SHUNT_COMPENSATOR_ERROR,
                        "Qmax at nominal voltage should be greater or equal to 0");
            }
            double newQatNominalV = modificationInfos.getMaxQAtNominalV().getValue() / maximumSectionCount;
            double newSusceptancePerSection = newQatNominalV / Math.pow(voltageLevel.getNominalV(), 2);
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldMaxQAtNominalV, modificationInfos.getMaxQAtNominalV().getValue(), "Qmax available at nominal voltage"));

            model.setBPerSection(shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? newSusceptancePerSection : -newSusceptancePerSection);
        }

        if (modificationInfos.getMaxSusceptance() != null) {
            double newSusceptancePerSection = modificationInfos.getMaxSusceptance().getValue() / maximumSectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldMaxSusceptance, modificationInfos.getMaxSusceptance().getValue(), "Maximal susceptance available"));

            model.setBPerSection(newSusceptancePerSection);
        }

        reportSwitchedOnAndPerSectionValues(reports, oldQAtNominalV, oldSwitchedOnQAtNominalV, oldSusceptancePerSection, oldSwitchedOnSusceptance, oldMaxQAtNominalV, sectionCount, maximumSectionCount);

        reports.forEach(report -> insertReportNode(subReportNode, report));
    }

    private void reportSwitchedOnAndPerSectionValues(List<ReportNode> reports, double oldQAtNominalV, double oldSwitchedOnQAtNominalV, double oldSusceptancePerSection, double oldSwitchedOnSusceptance, double oldMaxQAtNominalV, int sectionCount, int maximumSectionCount) {
        if (modificationInfos.getMaxQAtNominalV() != null) {
            double newQatNominalV = modificationInfos.getMaxQAtNominalV().getValue() / maximumSectionCount;
            double newSwitchedOnQAtNominalV = newQatNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQAtNominalV, newQatNominalV, "Q at nominal voltage per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        } else if (modificationInfos.getMaxSusceptance() != null) {
            double newSusceptancePerSection = modificationInfos.getMaxSusceptance().getValue() / maximumSectionCount;
            double newSwitchedOnSusceptance = newSusceptancePerSection * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSusceptancePerSection, newSusceptancePerSection, "Susceptance per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnSusceptance, newSwitchedOnSusceptance, "Switched-on susceptance"));
        } else if (modificationInfos.getMaximumSectionCount() != null) {
            double newQatNominalV = oldMaxQAtNominalV / maximumSectionCount;
            double newSwitchedOnQAtNominalV = newQatNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldQAtNominalV, newQatNominalV, "Q at nominal voltage per section"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        } else if (modificationInfos.getSectionCount() != null) {
            double newSwitchedOnQAtNominalV = oldQAtNominalV * sectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldSwitchedOnQAtNominalV, newSwitchedOnQAtNominalV, SWITCHED_ON_Q_AT_NOMINALV_LOG_MESSAGE));
        }
    }
}
