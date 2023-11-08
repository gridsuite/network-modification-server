/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
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
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class ShuntCompensatorModification extends AbstractModification {
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

        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND,
                    String.format("Voltage level %s does not exist in network", modificationInfos.getVoltageLevelId()));
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationInfos.getEquipmentId());
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId());

        subReporter.report(Report.builder()
                .withKey("shuntCompensatorModification")
                .withDefaultMessage("Shunt Compensator with id=${id} modified :")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        ModificationUtils.getInstance().applyElementaryModifications(shuntCompensator::setName, () -> shuntCompensator.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReporter, "Name");

        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.LINEAR) {
            applyModificationOnLinearModel(subReporter, shuntCompensator, voltageLevel);
        }
    }

    private void modifyMaximumSectionCount(List<Report> reports, ShuntCompensator shuntCompensator, ShuntCompensatorLinearModel model) {
        if (modificationInfos.getMaximumSectionCount() != null) {
            var maximumSectionCount = modificationInfos.getMaximumSectionCount().getValue();
            if (modificationInfos.getMaxSusceptance() == null && modificationInfos.getMaxQAtNominalV() == null) {
                model.setBPerSection(model.getBPerSection() * shuntCompensator.getMaximumSectionCount() / maximumSectionCount);
            }
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensator.getMaximumSectionCount(), maximumSectionCount, "Maximum section count"));
            model.setMaximumSectionCount(maximumSectionCount);
        }
    }

    private void modifySectionCount(List<Report> reports, ShuntCompensator shuntCompensator) {
        if (modificationInfos.getSectionCount() != null) {
            reports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(shuntCompensator::setSectionCount, shuntCompensator::getSectionCount, modificationInfos.getSectionCount(), "Section count"));
        }
    }

    private void applyModificationOnLinearModel(Reporter subReporter, ShuntCompensator shuntCompensator, VoltageLevel voltageLevel) {
        List<Report> reports = new ArrayList<>();
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        var shuntCompensatorType = model.getBPerSection() > 0 ? ShuntCompensatorType.CAPACITOR : ShuntCompensatorType.REACTOR;

        // due to cross validation between maximum section count and section count, we need to modify section count first
        // when maximum section count old value is greater than the new one
        if (modificationInfos.getMaximumSectionCount() != null && modificationInfos.getMaximumSectionCount().getValue() < shuntCompensator.getMaximumSectionCount()) {
            modifySectionCount(reports, shuntCompensator);
            modifyMaximumSectionCount(reports, shuntCompensator, model);
        } else {
            modifyMaximumSectionCount(reports, shuntCompensator, model);
            modifySectionCount(reports, shuntCompensator);
        }

        var maximumSectionCount = modificationInfos.getMaximumSectionCount() != null ? modificationInfos.getMaximumSectionCount().getValue() : shuntCompensator.getMaximumSectionCount();

        if (modificationInfos.getShuntCompensatorType() != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(shuntCompensatorType, modificationInfos.getShuntCompensatorType().getValue(), "Type"));
            shuntCompensatorType = modificationInfos.getShuntCompensatorType().getValue();
            if (modificationInfos.getMaxQAtNominalV() == null) {
                // we retrieve the absolute value of susceptance per section, then we determine the sign using the type
                double bPerSectionAbsoluteValue = Math.abs(model.getBPerSection());
                double newBPerSection = shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? bPerSectionAbsoluteValue : -bPerSectionAbsoluteValue;
                model.setBPerSection(newBPerSection);
            }
        }

        if (modificationInfos.getMaxQAtNominalV() != null) {
            double olQAtNominalV = Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * model.getBPerSection());
            double newQatNominalV = modificationInfos.getMaxQAtNominalV().getValue() / maximumSectionCount;
            double susceptancePerSection = newQatNominalV / Math.pow(voltageLevel.getNominalV(), 2);
            reports.add(ModificationUtils.getInstance().buildModificationReport(olQAtNominalV, newQatNominalV, "Q at nominal voltage"));
            model.setBPerSection(shuntCompensatorType == ShuntCompensatorType.CAPACITOR ? susceptancePerSection : -susceptancePerSection);
        }

        if (modificationInfos.getMaxSusceptance() != null) {
            double susceptancePerSection = modificationInfos.getMaxSusceptance().getValue() / maximumSectionCount;
            reports.add(ModificationUtils.getInstance().buildModificationReport(model.getBPerSection(), susceptancePerSection, "Susceptance per section"));
            model.setBPerSection(susceptancePerSection);
        }
        reports.forEach(subReporter::report);
    }
}
