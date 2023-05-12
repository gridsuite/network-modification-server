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
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.VoltageLevelModificationInfos;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class VoltageLevelModification extends AbstractModification {
    private final VoltageLevelModificationInfos modificationInfos;

    public VoltageLevelModification(VoltageLevelModificationInfos voltageLevelModificationInfos) {
        this.modificationInfos = voltageLevelModificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getEquipmentId());
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND,
                    String.format("Voltage level %s does not exist in network", modificationInfos.getEquipmentId()));
        }

        modifyVoltageLevel(subReporter, voltageLevel);
    }

    private void modifyVoltageLevel(Reporter subReporter, VoltageLevel voltageLevel) {
        subReporter.report(Report.builder()
                .withKey("voltageLevelModification")
                .withDefaultMessage("Voltage level with id=${id} modified :")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setName, voltageLevel::getNameOrId, modificationInfos.getEquipmentName(), subReporter, "Name");
        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setNominalV, voltageLevel::getNominalV, modificationInfos.getNominalVoltage(), subReporter, "Nominal voltage");
        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setLowVoltageLimit, voltageLevel::getLowVoltageLimit, modificationInfos.getLowVoltageLimit(), subReporter, "Low voltage limit");
        ModificationUtils.getInstance().applyElementaryModifications(voltageLevel::setHighVoltageLimit, voltageLevel::getHighVoltageLimit, modificationInfos.getHighVoltageLimit(), subReporter, "High voltage limit");

        modifyVoltageLevelShortCircuit(subReporter, voltageLevel);
    }

    private void modifyVoltageLevelShortCircuit(Reporter subReporter, VoltageLevel voltageLevel) {
        if (modificationInfos.getIpMin() != null || modificationInfos.getIpMax() != null) {
            List<Report> reports = new ArrayList<>();
            IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
            IdentifiableShortCircuitAdder<VoltageLevel> identifiableShortCircuitAdder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class);
            var oldIpMin = identifiableShortCircuit == null ? null : identifiableShortCircuit.getIpMin();
            var oldIpMax = identifiableShortCircuit == null ? null : identifiableShortCircuit.getIpMax();

            if (modificationInfos.getIpMin() != null) {
                var newIpMin = modificationInfos.getIpMin().getValue();
                identifiableShortCircuitAdder.withIpMin(newIpMin);
                reports.add(ModificationUtils.getInstance()
                        .buildModificationReport(oldIpMin, newIpMin, "Low short circuit current limit"));
            } else if (oldIpMin != null) {
                identifiableShortCircuitAdder.withIpMin(oldIpMin);
            }

            if (modificationInfos.getIpMax() != null) {
                var newIpMax = modificationInfos.getIpMax().getValue();
                identifiableShortCircuitAdder.withIpMax(newIpMax);
                reports.add(ModificationUtils.getInstance()
                        .buildModificationReport(oldIpMax, newIpMax, "High short circuit current limit"));
            } else if (oldIpMax != null) {
                identifiableShortCircuitAdder.withIpMax(oldIpMax);
            }

            identifiableShortCircuitAdder.add();
            reports.forEach(subReporter::report);
        }
    }
}
