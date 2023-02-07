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
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ShuntCompensatorCreation extends AbstractModification {

    private final ShuntCompensatorCreationInfos modificationInfos;

    public ShuntCompensatorCreation(ShuntCompensatorCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void control(Network network) throws NetworkModificationException {
        if (network.getShuntCompensator(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(SHUNT_COMPENSATOR_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }
        ModificationUtils.getInstance().controlInjectionCreation(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition());
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // create the shunt compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            ShuntCompensatorAdder shuntCompensatorAdder = createShuntAdderInNodeBreaker(voltageLevel, modificationInfos);
            var position = ModificationUtils.getInstance().getPosition(modificationInfos.getConnectionPosition(),
                    modificationInfos.getBusOrBusbarSectionId(), network, voltageLevel);
            CreateFeederBay algo = new CreateFeederBayBuilder()
                    .withBbsId(modificationInfos.getBusOrBusbarSectionId())
                    .withInjectionDirection(modificationInfos.getConnectionDirection())
                    .withInjectionFeederName(modificationInfos.getConnectionName())
                    .withInjectionPositionOrder(position)
                    .withInjectionAdder(shuntCompensatorAdder)
                    .build();
            algo.apply(network, true, subReporter);
        } else {
            createShuntInBusBreaker(voltageLevel, modificationInfos);
            subReporter.report(Report.builder()
                    .withKey("shuntCompensatorCreated")
                    .withDefaultMessage("New shunt compensator with id=${id} created")
                    .withValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    private ShuntCompensatorAdder createShuntAdderInNodeBreaker(VoltageLevel voltageLevel, ShuntCompensatorCreationInfos shuntCompensatorInfos) {
        // creating the shunt compensator
        ShuntCompensatorAdder shuntAdder = voltageLevel.newShuntCompensator()
                .setId(shuntCompensatorInfos.getEquipmentId())
                .setName(shuntCompensatorInfos.getEquipmentName())
                .setSectionCount(shuntCompensatorInfos.getCurrentNumberOfSections());

        /* when we create non linear shunt, this is where we branch ;) */
        shuntAdder.newLinearModel()
                .setBPerSection(shuntCompensatorInfos.getSusceptancePerSection())
                .setMaximumSectionCount(shuntCompensatorInfos.getMaximumNumberOfSections()).add();

        return shuntAdder;
    }

    private void createShuntInBusBreaker(VoltageLevel voltageLevel, ShuntCompensatorCreationInfos shuntCompensatorInfos) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, shuntCompensatorInfos.getBusOrBusbarSectionId());
        /* creating the shunt compensator */
        voltageLevel.newShuntCompensator()
            .setId(shuntCompensatorInfos.getEquipmentId())
            .setName(shuntCompensatorInfos.getEquipmentName())
            .setSectionCount(shuntCompensatorInfos.getCurrentNumberOfSections())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .newLinearModel()
            .setBPerSection(shuntCompensatorInfos.getSusceptancePerSection())
            .setMaximumSectionCount(shuntCompensatorInfos.getMaximumNumberOfSections())
            .add()
            .add();
    }
}
