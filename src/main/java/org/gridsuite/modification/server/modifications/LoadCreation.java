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
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.NetworkModification;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.dto.LoadCreationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LoadCreation implements NetworkModification {

    private final LoadCreationInfos modificationInfos;

    public LoadCreation(LoadCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, ComputationManager computationManager) {
        apply(network);
    }

    @Override
    public void apply(Network network) {
        apply(network, Reporter.NO_OP);
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // create the load in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createLoadInNodeBreaker(voltageLevel);
        } else {
            createLoadInBusBreaker(voltageLevel);
        }

        subReporter.report(Report.builder()
            .withKey("loadCreated")
            .withDefaultMessage("New load with id=${id} created")
            .withValue("id", modificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    private void createLoadInNodeBreaker(VoltageLevel voltageLevel) {
        // create cell switches
        int nodeNum = ModificationUtils.getInstance().createNodeBreakerCellSwitches(voltageLevel, modificationInfos.getBusOrBusbarSectionId(),
            modificationInfos.getEquipmentId(),
            modificationInfos.getEquipmentName());

        // creating the load
        voltageLevel.newLoad()
            .setId(modificationInfos.getEquipmentId())
            .setName(modificationInfos.getEquipmentName())
            .setLoadType(modificationInfos.getLoadType())
            .setNode(nodeNum)
            .setP0(modificationInfos.getActivePower())
            .setQ0(modificationInfos.getReactivePower())
            .add();
    }

    private void createLoadInBusBreaker(VoltageLevel voltageLevel) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, modificationInfos.getBusOrBusbarSectionId());

        // creating the load
        voltageLevel.newLoad()
            .setId(modificationInfos.getEquipmentId())
            .setName(modificationInfos.getEquipmentName())
            .setLoadType(modificationInfos.getLoadType())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(modificationInfos.getActivePower())
            .setQ0(modificationInfos.getReactivePower())
            .add();
    }
}
