/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.MinMaxReactiveLimits;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurve;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.VscConverterStation;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRange;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.dto.VscCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_VSC_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.HVDC_LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
                .p0(5F)
                .operatorActivePowerLimitFromSide2ToSide1(5.6F)
                .convertersMode(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)
                .activePower(5.)
                .operatorActivePowerLimitFromSide1ToSide2(6.0F)
                .operatorActivePowerLimitFromSide2ToSide1(8F)
                .droop(1F)
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
                .voltage(34.)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1.1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .reactiveCapabilityCurvePoints(List.of())
                .reactiveCapabilityCurve(false)
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
                .reactivePower(44.)
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
        assertNotNull(getNetwork().getHvdcLine("vsc1"));

        assertEquals(1, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId1")).count());

        assertEquals(1, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId2")).count());

        HvdcLine hvdcLine = getNetwork().getHvdcLine("vsc1");
        assertNotNull(hvdcLine);
        assertEquals(hvdcLine.getConvertersMode(), HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER);
        assertEquals(hvdcLine.getNominalV(), 39, 0);
        assertEquals(hvdcLine.getR(), 4, 0);
        assertEquals(hvdcLine.getActivePowerSetpoint(), 5, 0);
        assertEquals(hvdcLine.getMaxP(), 56, 0);

        HvdcOperatorActivePowerRange hvdcOperatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
        assertEquals(hvdcOperatorActivePowerRange.getOprFromCS1toCS2(), 6, 0);
        assertEquals(hvdcOperatorActivePowerRange.getOprFromCS2toCS1(), 8, 0);

        HvdcAngleDroopActivePowerControl activePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertEquals(activePowerControl.getDroop(), 1, 0);
        assertEquals(activePowerControl.getP0(), 5, 0);

        VscConverterStation vscConverterStation1 = (VscConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(vscConverterStation1);
        assertEquals(vscConverterStation1.getReactivePowerSetpoint(), 44, 0);
        assertEquals(vscConverterStation1.getLossFactor(), 40, 0);
        assertEquals(vscConverterStation1.getReactiveLimits().getKind(), ReactiveLimitsKind.CURVE);
        ReactiveCapabilityCurve reactiveLimits1 = vscConverterStation1.getReactiveLimits(ReactiveCapabilityCurve.class);
        assertEquals(reactiveLimits1.getPointCount(), 2);
        assertEquals(reactiveLimits1.getMaxP(), 0.6, 0);
        assertEquals(reactiveLimits1.getMinP(), 0.4, 0);
        assertEquals(vscConverterStation1.getVoltageSetpoint(), 66, 0);
        assertEquals(vscConverterStation1.getTerminal().getVoltageLevel().getId(), "v1");

        VscConverterStation vscConverterStation2 = (VscConverterStation) hvdcLine.getConverterStation2();
        assertNotNull(vscConverterStation2);
        assertEquals(vscConverterStation2.getReactivePowerSetpoint(), 23, 0);
        assertEquals(vscConverterStation2.getLossFactor(), 4, 0);
        assertEquals(vscConverterStation2.getReactiveLimits().getKind(), ReactiveLimitsKind.MIN_MAX);
        MinMaxReactiveLimits reactiveLimits2 = vscConverterStation2.getReactiveLimits(MinMaxReactiveLimits.class);
        assertEquals(reactiveLimits2.getMaxQ(), 66, 0);
        assertEquals(reactiveLimits2.getMinQ(), 55, 0);
        assertEquals(vscConverterStation2.getVoltageSetpoint(), 34, 0);
        assertEquals(vscConverterStation2.getTerminal().getVoltageLevel().getId(), "v2");
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getHvdcLine("vsc1"));

        assertEquals(0, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId1")).count());

        assertEquals(0, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId2")).count());
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        VscCreationInfos vscCreationInfos = (VscCreationInfos) buildModification();
        vscCreationInfos.setEquipmentId("");
        String vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", vscCreationInfos.getErrorType().name(), reportService);

        // not found voltage level
        vscCreationInfos.setEquipmentId("vscId");
        ConverterStationCreationInfos converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        vscCreationInfos.setConverterStation2(converterStationCreationInfos);
        vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                vscCreationInfos.getErrorType().name(), reportService);

        // invalid min max reactive limit
        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setConnectionPosition(35);
        converterStationCreationInfos.setReactiveCapabilityCurve(false);
        converterStationCreationInfos.setMinimumReactivePower(Double.NaN);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : minimum reactive power is not set").getMessage(),
                vscCreationInfos.getErrorType().name(), reportService);

        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setConnectionPosition(66);
        converterStationCreationInfos.setReactiveCapabilityCurve(false);
        converterStationCreationInfos.setMaximumReactivePower(Double.NaN);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : maximum reactive power is not set").getMessage(),
                vscCreationInfos.getErrorType().name(), reportService);

        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithMinMaxReactiveLimits();
        converterStationCreationInfos.setConnectionPosition(15);
        converterStationCreationInfos.setReactiveCapabilityCurve(false);
        converterStationCreationInfos.setMinimumReactivePower(200.);
        converterStationCreationInfos.setMaximumReactivePower(100.);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : maximum reactive power is expected to be greater than or equal to minimum reactive power").getMessage(),
                vscCreationInfos.getErrorType().name(), reportService);

        // invalid reactive capability curve limit
        vscCreationInfos = (VscCreationInfos) buildModification();
        converterStationCreationInfos = buildConverterStationWithReactiveCapabilityCurve();
        converterStationCreationInfos.setConnectionPosition(55);
        converterStationCreationInfos.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);
        vscCreationInfos.setConverterStation1(converterStationCreationInfos);

        vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VSC_ERROR, "Vsc 'vsc1' : P is not set in a reactive capability curve limits point").getMessage(),
                vscCreationInfos.getErrorType().name(), reportService);

        // try to create an existing vsc
        vscCreationInfos = (VscCreationInfos) buildModification();
        vscCreationInfos.setEquipmentId("hvdcLine");
        vscCreationInfosJson = mapper.writeValueAsString(vscCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(vscCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, "hvdcLine").getMessage(),
                vscCreationInfos.getErrorType().name(), reportService);
    }
}
