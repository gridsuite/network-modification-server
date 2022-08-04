/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LoadCreationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class GeneratorCreation extends AbstractModification {

    public GeneratorCreation(LoadCreationInfos modificationInfos) {
        super(modificationInfos);
    }

    private LoadCreationInfos getModificationInfos() {
        return (LoadCreationInfos) modificationInfos;
    }

    @Override
    protected ModificationType getType() {
        return ModificationType.LOAD_CREATION;
    }

    @Override
    protected NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.CREATE_LOAD_ERROR;
    }

    @Override
    protected Reporter createSubReporter(ReporterModel reporter) {
        String subReportId = "Load creation " + getModificationInfos().getEquipmentId();
        return reporter.createSubReporter(subReportId, subReportId);
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // create the load in the network
        VoltageLevel voltageLevel = getVoltageLevel(network, getModificationInfos().getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createLoadInNodeBreaker(voltageLevel);
        } else {
            createLoadInBusBreaker(voltageLevel);
        }

        subReporter.report(Report.builder()
            .withKey("loadCreated")
            .withDefaultMessage("New load with id=${id} created")
            .withValue("id", getModificationInfos().getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    private void createLoadInNodeBreaker(VoltageLevel voltageLevel) {
        // create cell switches
        int nodeNum = createNodeBreakerCellSwitches(voltageLevel, getModificationInfos().getBusOrBusbarSectionId(),
            getModificationInfos().getEquipmentId(),
            getModificationInfos().getEquipmentName());

        // creating the load
        voltageLevel.newLoad()
            .setId(getModificationInfos().getEquipmentId())
            .setName(getModificationInfos().getEquipmentName())
            .setLoadType(getModificationInfos().getLoadType())
            .setNode(nodeNum)
            .setP0(getModificationInfos().getActivePower())
            .setQ0(getModificationInfos().getReactivePower())
            .add();
    }

    private void createLoadInBusBreaker(VoltageLevel voltageLevel) {
        Bus bus = getBusBreakerBus(voltageLevel, getModificationInfos().getBusOrBusbarSectionId());

        // creating the load
        voltageLevel.newLoad()
            .setId(getModificationInfos().getEquipmentId())
            .setName(getModificationInfos().getEquipmentName())
            .setLoadType(getModificationInfos().getLoadType())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(getModificationInfos().getActivePower())
            .setQ0(getModificationInfos().getReactivePower())
            .add();
    }

}
