/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.dto.VscCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

public class VscCreationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return VscCreationInfos.builder()
                .equipmentId("vsc1")
                .equipmentName("vsc1Name")
                .dcNominalVoltage(39.)
                .dcResistance(4.)
                .maximumActivePower(56.)
                .p0(8.7F)
                .operatorActivePowerLimitFromSide2ToSide1(5.6F)
                .convertersMode(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)
                .activePower(5.)
                .operatorActivePowerLimitFromSide1ToSide2(6.0F)
                .operatorActivePowerLimitFromSide2ToSide1(8.7F)
                .droop(1.1F)
                .angleDroopActivePowerControl(false)
                .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
                .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
                .build();
    }

    private ConverterStationCreationInfos buildConverterStationWithMinMaxReactiveLimits() {
        return ConverterStationCreationInfos.builder()
                .equipmentId("stationId2")
                .equipmentName("station2")
                .voltageRegulationOn(false)
                .reactivePower(23.)
                .reactiveCapabilityCurve(false)
                .maximumReactivePower(66.)
                .lossFactor(4F)
                .minimumReactivePower(55.)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1.1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .reactiveCapabilityCurve(false)
                .reactiveCapabilityCurvePoints(List.of())
                .build();
    }

    private ConverterStationCreationInfos buildConverterStationWithReactiveCapabilityCurve() {
        var point1 = ReactiveCapabilityCurveCreationInfos.builder()
                .p(0.4)
                .qmaxP(3.)
                .qminP(0.)
                .build();
        var point2 = ReactiveCapabilityCurveCreationInfos.builder()
                .p(0.6)
                .qmaxP(2.)
                .qminP(1.1)
                .build();

        return ConverterStationCreationInfos.builder()
                .equipmentId("stationId1")
                .equipmentName("station1")
                .voltageRegulationOn(true)
                .voltage(66.)
                .lossFactor(40F)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(List.of(point1, point2))
                .voltageLevelId("v1")
                .busOrBusbarSectionId("1.1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VscCreationInfos.builder()
                .equipmentId("vsc1")
                .equipmentName("vsc2Name")
                .dcNominalVoltage(53.)
                .dcResistance(2.)
                .maximumActivePower(77.)
                .p0(8.3F)
                .operatorActivePowerLimitFromSide2ToSide1(5.2F)
                .convertersMode(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER)
                .activePower(7.)
                .operatorActivePowerLimitFromSide1ToSide2(6.1F)
                .operatorActivePowerLimitFromSide2ToSide1(8.3F)
                .angleDroopActivePowerControl(true)
                .droop(2.1F)
                .converterStation1(buildConverterStationWithMinMaxReactiveLimits())
                .converterStation2(buildConverterStationWithReactiveCapabilityCurve())
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        var list = getNetwork().getHvdcLineStream().toList();
        assertNotNull(getNetwork().getHvdcLine("vsc1"));

        assertEquals(1, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(transformer -> transformer.getId().equals("stationId1")).count());

        assertEquals(1, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(transformer -> transformer.getId().equals("stationId2")).count());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getHvdcLine("vsc1"));

        assertEquals(0, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(transformer -> transformer.getId().equals("stationId1")).count());

        assertEquals(0, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(transformer -> transformer.getId().equals("stationId2")).count());
    }
}
