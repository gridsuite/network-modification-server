/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.error.NetworkModificationExceptionType.HVDC_LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.error.NetworkModificationExceptionType.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LCC_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("lcc1", createdValues.get("equipmentId"));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        LccCreationInfos lccCreationInfos = (LccCreationInfos) buildModification();
        lccCreationInfos.setEquipmentId("");
        String lccCreationInfosJson = getJsonBody(lccCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lccCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", ERROR_MESSAGE_KEY, reportService);

        // not found voltage level
        lccCreationInfos.setEquipmentId("lccId");
        LccConverterStationCreationInfos converterStationCreationInfos = buildLccConverterStation("lccStationId1",
                "lccStationName1", "v1", "1.1");
        converterStationCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        lccCreationInfos.setConverterStation2(converterStationCreationInfos);
        lccCreationInfosJson = getJsonBody(lccCreationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(lccCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                ERROR_MESSAGE_KEY, reportService);

        // try to create an existing lcc
        lccCreationInfos = (LccCreationInfos) buildModification();
        lccCreationInfos.setEquipmentId("hvdcLine");
        lccCreationInfosJson = getJsonBody(lccCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lccCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, "hvdcLine").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }
}
