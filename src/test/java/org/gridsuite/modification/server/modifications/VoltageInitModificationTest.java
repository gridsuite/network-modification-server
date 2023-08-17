/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.ThreeWindingsTransformer;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.VoltageInitGeneratorModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitTransformerModificationInfos;
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
public class VoltageInitModificationTest extends AbstractNetworkModificationTest {
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

        ThreeWindingsTransformer transformer = network.getThreeWindingsTransformer("trf6");
        transformer.getLeg2().newRatioTapChanger()
            .setLowTapPosition(0)
            .setTapPosition(1)
            .setLoadTapChangingCapabilities(false)
            .setRegulating(true)
            .setTargetDeadband(1.0)
            .setTargetV(220.0)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .endStep()
            .beginStep()
            .setR(39.78474)
            .setX(39.784726)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .endStep()
            .add();

        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return VoltageInitModificationInfos.builder()
            .generators(List.of(
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("idGenerator")
                    .reactivePowerSetpoint(10.)
                    .build(),
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("newGen")
                    .voltageSetpoint(226.)
                    .build()))
            .transformers(List.of(
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf1")
                    .ratioTapChangerPosition(2)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf2")
                    .ratioTapChangerPosition(2)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("2wtNotFound")
                    .ratioTapChangerPosition(2)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(2)
                    .legSide(ThreeWindingsTransformer.Side.TWO)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("3wtNotFound")
                    .legSide(ThreeWindingsTransformer.Side.THREE)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("3wtNotFound")
                    .ratioTapChangerPosition(1)
                    .legSide(ThreeWindingsTransformer.Side.ONE)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(1)
                    .legSide(ThreeWindingsTransformer.Side.ONE)
                    .build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VoltageInitModificationInfos.builder()
            .generators(List.of(
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("idGenerator")
                    .voltageSetpoint(370.)
                    .build(),
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("v5generator")
                    .reactivePowerSetpoint(15.)
                    .build()))
                        .transformers(List.of(
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf1")
                    .ratioTapChangerPosition(1)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("trf6")
                    .ratioTapChangerPosition(2)
                    .legSide(ThreeWindingsTransformer.Side.TWO)
                    .build()))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(10., getNetwork().getGenerator("idGenerator").getTargetQ(), 0.001);
        assertEquals(226., getNetwork().getGenerator("newGen").getTargetV(), 0.001);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(1., getNetwork().getGenerator("idGenerator").getTargetQ(), 0.001);
        assertEquals(224., getNetwork().getGenerator("newGen").getTargetV(), 0.001);
    }
}
