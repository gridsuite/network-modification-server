/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LoadCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_ALREADY_EXISTS;
import static org.gridsuite.modification.server.modifications.ModificationUtils.createInjectionInNodeBreaker;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LoadCreation extends AbstractModification {

    private final LoadCreationInfos modificationInfos;

    public LoadCreation(LoadCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLoad(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(LOAD_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        ModificationUtils.getInstance().controlConnectivity(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition());
    }

    @Override
    public void apply(Network network, ReportNode subReporter) {
        // create the load in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            LoadAdder loadAdder = createLoadAdderInNodeBreaker(voltageLevel, modificationInfos);
            createInjectionInNodeBreaker(voltageLevel, modificationInfos, network, loadAdder, subReporter);
        } else {
            createLoadInBusBreaker(voltageLevel, modificationInfos);
            subReporter.newReportNode()
                .withMessageTemplate("loadCreated", "New load with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        }
        reportElementaryCreations(subReporter);
        ModificationUtils.getInstance().disconnectCreatedInjection(modificationInfos, network.getLoad(modificationInfos.getEquipmentId()), subReporter);

        // properties
        Load load = network.getLoad(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(load, subReporter, modificationInfos.getProperties(), "LoadProperties");
    }

    @Override
    public String getName() {
        return "LoadCreation";
    }

    private void reportElementaryCreations(ReportNode subReportNode) {
        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getEquipmentName(), "Name");
        }

        if (modificationInfos.getLoadType() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getLoadType(), "Type");
        }

        ModificationUtils.getInstance()
                .reportElementaryCreation(subReportNode, modificationInfos.getP0(), "Active power");

        ModificationUtils.getInstance()
                .reportElementaryCreation(subReportNode, modificationInfos.getQ0(), "Reactive power");
    }

    private LoadAdder createLoadAdderInNodeBreaker(VoltageLevel voltageLevel, LoadCreationInfos loadCreationInfos) {
        // creating the load adder
        return voltageLevel.newLoad()
            .setId(loadCreationInfos.getEquipmentId())
            .setName(loadCreationInfos.getEquipmentName())
            .setLoadType(loadCreationInfos.getLoadType())
            .setP0(loadCreationInfos.getP0())
            .setQ0(loadCreationInfos.getQ0());
    }

    private Load createLoadInBusBreaker(VoltageLevel voltageLevel, LoadCreationInfos loadCreationInfos) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, loadCreationInfos.getBusOrBusbarSectionId());

        // creating the load
        return voltageLevel.newLoad()
            .setId(loadCreationInfos.getEquipmentId())
            .setName(loadCreationInfos.getEquipmentName())
            .setLoadType(loadCreationInfos.getLoadType())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(loadCreationInfos.getP0())
            .setQ0(loadCreationInfos.getQ0()).add();
    }
}
