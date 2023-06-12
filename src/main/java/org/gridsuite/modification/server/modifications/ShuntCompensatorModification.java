/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;

import static org.gridsuite.modification.server.NetworkModificationException.Type.SHUNT_COMPENSATOR_NOT_FOUND;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class ShuntCompensatorModification extends AbstractModification {
    private final ShuntCompensatorModificationInfos modificationInfos;

    public ShuntCompensatorModification(ShuntCompensatorModificationInfos shuntCompensatorModificationInfos) {
        this.modificationInfos = shuntCompensatorModificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        ShuntCompensator shuntCompensator = network.getShuntCompensator(modificationInfos.getEquipmentId());
        if (shuntCompensator == null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_NOT_FOUND,
                    String.format("Shunt compensator %s does not exist in network", modificationInfos.getEquipmentId()));
        }
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getVoltageLevelId().getValue());

        ModificationUtils.getInstance().applyElementaryModifications(shuntCompensator::setName, shuntCompensator::getNameOrId, modificationInfos.getEquipmentName(), subReporter, "Name");

        if (shuntCompensator.getModelType() == ShuntCompensatorModelType.LINEAR) {
            applyModificationOnLinearModel(subReporter, shuntCompensator, voltageLevel);
        }

    }

    private void applyModificationOnLinearModel(Reporter subReporter, ShuntCompensator shuntCompensator, VoltageLevel voltageLevel) {
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);

        if (modificationInfos.getQAtNominalV() != null) {
            var olQAtNominalV = Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * model.getBPerSection());

            var oldType = model.getBPerSection() > 0 ? ShuntCompensatorType.CAPACITOR : ShuntCompensatorType.REACTOR;
            var type = modificationInfos.getShuntCompensatorType() != null ?
                    modificationInfos.getShuntCompensatorType().getValue() : oldType;

            Double susceptancePerSection = modificationInfos.getQAtNominalV().getValue() / Math.pow(voltageLevel.getNominalV(), 2);

            model.setBPerSection(type == ShuntCompensatorType.CAPACITOR ? susceptancePerSection : -susceptancePerSection);
            subReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(olQAtNominalV, modificationInfos.getQAtNominalV(), "Q at nominal voltage", 2));
        }

        ModificationUtils.getInstance().applyElementaryModifications(model::setBPerSection, model::getBPerSection, modificationInfos.getSusceptancePerSection(), subReporter, "Susceptance per section");
    }
}
