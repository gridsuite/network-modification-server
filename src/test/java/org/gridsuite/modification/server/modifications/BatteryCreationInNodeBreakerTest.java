/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class BatteryCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new battery in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId("idBattery1")
                .equipmentName("idBattery1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .minP(100.0)
                .maxP(600.0)
                .targetP(400.)
                .targetQ(50.)
                .minQ(20.0)
                .maxQ(25.0)
                .droop(5f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurveCreationInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId("idBattery2Edited")
                .equipmentName("nameBatteryModified")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .minP(101.0)
                .maxP(601.0)
                .targetP(401.)
                .targetQ(51.)
                .minQ(23.0)
                .maxQ(26.0)
                .droop(6f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(1.0, 2.0, 2.1),
                        new ReactiveCapabilityCurveCreationInfos(6.6, 8.8, 11.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getBattery("idBattery1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("idBattery1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getBattery("idBattery1"));
        assertEquals(0, getNetwork().getVoltageLevel("v2").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery1")).count());
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // invalid Battery id
        BatteryCreationInfos batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setEquipmentId("");
        String batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", batteryCreationInfos.getErrorType().name(), reportService);

        // not found voltage level
        batteryCreationInfos.setEquipmentId("idBattery1");
        batteryCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                batteryCreationInfos.getErrorType().name(), reportService);

        // not found busbar section
        batteryCreationInfos.setVoltageLevelId("v2");
        batteryCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage(),
                batteryCreationInfos.getErrorType().name(), reportService);

        // invalid min active power
        batteryCreationInfos.setVoltageLevelId("v2");

        batteryCreationInfos.setBusOrBusbarSectionId("1B");
        batteryCreationInfos.setMinP(Double.NaN);
        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Battery 'idBattery1': invalid value (NaN) for minimum P",
                batteryCreationInfos.getErrorType().name(), reportService);

        // invalid min max reactive limit
        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setReactiveCapabilityCurve(false);
        batteryCreationInfos.setMinQ(Double.NaN);

        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery 'idBattery1' : minimum reactive power is not set").getMessage(),
            batteryCreationInfos.getErrorType().name(), reportService);

        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setReactiveCapabilityCurve(false);
        batteryCreationInfos.setMaxQ(Double.NaN);

        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery 'idBattery1' : maximum reactive power is not set").getMessage(),
            batteryCreationInfos.getErrorType().name(), reportService);

        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setReactiveCapabilityCurve(false);
        batteryCreationInfos.setMinQ(200.);
        batteryCreationInfos.setMaxQ(100.);

        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery 'idBattery1' : maximum reactive power is expected to be greater than or equal to minimum reactive power").getMessage(),
            batteryCreationInfos.getErrorType().name(), reportService);

        // invalid reactive capability curve limit
        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);

        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery 'idBattery1' : P is not set in a reactive capability curve limits point").getMessage(),
            batteryCreationInfos.getErrorType().name(), reportService);

        // try to create an existing battery
        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setEquipmentId("v3Battery");
        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BATTERY_ALREADY_EXISTS, "v3Battery").getMessage(),
                batteryCreationInfos.getErrorType().name(), reportService);
        batteryCreationInfos.setEquipmentId("idBattery3");
        batteryCreationInfos.setEquipmentName("nameBattery3");
        batteryCreationInfos.setVoltageLevelId("v2");
        batteryCreationInfos.setBusOrBusbarSectionId("1B");
        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationResult.isEmpty());  // no modifications returned
        assertNull(getNetwork().getBattery("idBattery3"));  // battery was not created
        testNetworkModificationsCount(getGroupId(), 10);  // new modification stored in the database
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery1", updatedValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery2Edited", updatedValues.get("equipmentId"));
    }
}
