/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBays;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBaysBuilder;
import com.powsybl.iidm.network.LineAdder;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LineCreation extends AbstractModification {

    private final LineCreationInfos modificationInfos;

    public LineCreation(LineCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {

        // create the line in the network
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());

        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LineAdder lineAdder = ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, modificationInfos, false, false);
            var position1 = modificationInfos.getConnectionPosition1() != null ? modificationInfos.getConnectionPosition1() : ModificationUtils.getInstance().getPosition(modificationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
            var position2 = modificationInfos.getConnectionPosition2() != null ? modificationInfos.getConnectionPosition2() : ModificationUtils.getInstance().getPosition(modificationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

            CreateBranchFeederBays algo = new CreateBranchFeederBaysBuilder()
                    .withBusOrBusbarSectionId1(modificationInfos.getBusOrBusbarSectionId1())
                    .withBusOrBusbarSectionId2(modificationInfos.getBusOrBusbarSectionId2())
                    .withFeederName1(modificationInfos.getConnectionName1() != null ? modificationInfos.getConnectionName1() : modificationInfos.getEquipmentId())
                    .withFeederName2(modificationInfos.getConnectionName2() != null ? modificationInfos.getConnectionName2() : modificationInfos.getEquipmentId())
                    .withDirection1(modificationInfos.getConnectionDirection1())
                    .withDirection2(modificationInfos.getConnectionDirection2())
                    .withPositionOrder1(position1)
                    .withPositionOrder2(position2)
                    .withBranchAdder(lineAdder).build();
            algo.apply(network, true, subReporter);
        } else {
            addLine(network, voltageLevel1, voltageLevel2, modificationInfos, true, true, subReporter);
        }

        // Set Permanent Current Limits if exist
        CurrentLimitsInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        var line = ModificationUtils.getInstance().getLine(network, modificationInfos.getEquipmentId());

        if (currentLimitsInfos1 != null && currentLimitsInfos1.getPermanentLimit() != null) {
            line.newCurrentLimits1().setPermanentLimit(currentLimitsInfos1.getPermanentLimit()).add();
        }
        if (currentLimitsInfos2 != null && currentLimitsInfos2.getPermanentLimit() != null) {
            line.newCurrentLimits2().setPermanentLimit(currentLimitsInfos2.getPermanentLimit()).add();
        }
    }

    private void addLine(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2, Reporter subReporter) {
        ModificationUtils.getInstance().createLineAdder(network, voltageLevel1, voltageLevel2, lineCreationInfos, withSwitch1, withSwitch2).add();

        subReporter.report(Report.builder()
                .withKey("lineCreated")
                .withDefaultMessage("New line with id=${id} created")
                .withValue("id", lineCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

}
