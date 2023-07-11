/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.MassiveEquipmentsModificationsInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSwitch;
import static org.junit.Assert.assertEquals;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
public class MassiveEquipmentsModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);

        VoltageLevel v2 = network.getVoltageLevel("v2");
        createGenerator(v2, "newGen", 18, 45., 1.0, "feederNewGen", 50, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "dNewGen", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 19);
        createSwitch(v2, "brNewGen", null, SwitchKind.BREAKER, true, false, false, 18, 19);

        Generator newGen = network.getGenerator("newGen");
        newGen.setTargetV(224.);
        newGen.setVoltageRegulatorOn(true);

        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return MassiveEquipmentsModificationsInfos.builder()
            .modifications(List.of(
                GeneratorModificationInfos.builder()
                    .equipmentId("idGenerator")
                    .reactivePowerSetpoint(new AttributeModification<>(10., OperationType.SET))
                    .build(),
                GeneratorModificationInfos.builder()
                    .equipmentId("newGen")
                    .voltageSetpoint(new AttributeModification<>(226., OperationType.SET))
                    .build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return MassiveEquipmentsModificationsInfos.builder()
            .modifications(List.of(
                GeneratorModificationInfos.builder()
                    .equipmentId("idGenerator")
                    .voltageSetpoint(new AttributeModification<>(370., OperationType.SET))
                    .build(),
                GeneratorModificationInfos.builder()
                    .equipmentId("v5generator")
                    .reactivePowerSetpoint(new AttributeModification<>(15., OperationType.SET))
                    .build()))
            .build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(10., getNetwork().getGenerator("idGenerator").getTargetQ(), 0.001);
        assertEquals(226., getNetwork().getGenerator("newGen").getTargetV(), 0.001);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(1., getNetwork().getGenerator("idGenerator").getTargetQ(), 0.001);
        assertEquals(224., getNetwork().getGenerator("newGen").getTargetV(), 0.001);
    }
}
