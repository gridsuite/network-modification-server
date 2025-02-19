/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.LccConverterStation;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.HVDC_LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
class LccCreationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LccCreationInfos.builder()
                .stashed(false)
                .equipmentId("lcc1")
                .equipmentName("lcc1Name")
                .nominalV(39.)
                .r(4.)
                .maxP(56.)
                .activePowerSetpoint(5.)
                .convertersMode(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)
                .converterStation1(buildLccConverterStation("lccStationId1", "lccStationName1", "v1", "1.1"))
                .converterStation2(buildLccConverterStation("lccStationId2", "lccStationName2", "v2", "1B"))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    private static LccConverterStationCreationInfos buildLccConverterStation(String equipmentId, String equipmentName, String voltageLevel, String busOrBusbarSectionId) {
        return LccConverterStationCreationInfos.builder()
                .equipmentId(equipmentId)
                .equipmentName(equipmentName)
                .lossFactor(40F)
                .powerFactor(1F)
                .shuntCompensatorsOnSide(List.of())
                .voltageLevelId(voltageLevel)
                .busOrBusbarSectionId(busOrBusbarSectionId)
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LccCreationInfos.builder()
                .stashed(false)
                .equipmentId("lcc1Edited")
                .equipmentName("lcc2Name")
                .nominalV(53.)
                .r(2.)
                .maxP(77.)
                .convertersMode(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER)
                .activePowerSetpoint(7.)
                .converterStation1(buildLccConverterStation("lccStationId1", "lccStationName1", "v1", "1.1"))
                .converterStation2(buildLccConverterStation("lccStationId2", "lccStationName2", "v2", "1B"))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getHvdcLine("lcc1"));

        assertEquals(1, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("lccStationId1")).count());

        assertEquals(1, getNetwork().getVoltageLevel("v2").getLccConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("lccStationId2")).count());

        HvdcLine hvdcLine = getNetwork().getHvdcLine("lcc1");
        assertNotNull(hvdcLine);
        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());
        assertEquals(39, hvdcLine.getNominalV(), 0);
        assertEquals(4, hvdcLine.getR(), 0);
        assertEquals(PROPERTY_VALUE, hvdcLine.getProperty(PROPERTY_NAME));
        LccConverterStation lccConverterStation1 = (LccConverterStation) hvdcLine.getConverterStation1();
        assertNotNull(lccConverterStation1);
        assertEquals(40, lccConverterStation1.getLossFactor(), 0);
        assertEquals("v1", lccConverterStation1.getTerminal().getVoltageLevel().getId());
        LccConverterStation lccConverterStation2 = (LccConverterStation) hvdcLine.getConverterStation2();
        assertNotNull(lccConverterStation2);
        assertEquals(40, lccConverterStation2.getLossFactor(), 0);
        assertEquals("v2", lccConverterStation2.getTerminal().getVoltageLevel().getId());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LCC_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("lcc1", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LCC_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("lcc1Edited", updatedValues.get("equipmentId"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getHvdcLine("lcc1"));

        assertEquals(0, getNetwork().getVoltageLevel("v1").getLccConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId1")).count());

        assertEquals(0, getNetwork().getVoltageLevel("v2").getLccConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId2")).count());
    }

    @Test
    void testCreateWithErrors() throws Exception {
        LccCreationInfos lccCreationInfos = (LccCreationInfos) buildModification();
        lccCreationInfos.setEquipmentId("");
        String lccCreationInfosJson = mapper.writeValueAsString(lccCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lccCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", lccCreationInfos.getErrorType().name(), reportService);

        // not found voltage level
        lccCreationInfos.setEquipmentId("lccId");
        LccConverterStationCreationInfos converterStationCreationInfos = buildLccConverterStation("lccStationId1",
                "lccStationName1", "v1", "1.1");
        converterStationCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        lccCreationInfos.setConverterStation2(converterStationCreationInfos);
        lccCreationInfosJson = mapper.writeValueAsString(lccCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lccCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                lccCreationInfos.getErrorType().name(), reportService);

        // try to create an existing lcc
        lccCreationInfos = (LccCreationInfos) buildModification();
        lccCreationInfos.setEquipmentId("hvdcLine");
        lccCreationInfosJson = mapper.writeValueAsString(lccCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lccCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, "hvdcLine").getMessage(),
                lccCreationInfos.getErrorType().name(), reportService);
    }
}
