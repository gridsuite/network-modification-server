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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class BatteryCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new battery in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return BatteryCreationInfos.builder()
                .equipmentId("idBattery1")
                .equipmentName("idBattery1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .minActivePower(100.0)
                .maxActivePower(600.0)
                .activePowerSetpoint(400.)
                .reactivePowerSetpoint(50.)
                .minimumReactivePower(20.0)
                .maximumReactivePower(25.0)
                .droop(5f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurveCreationInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BatteryCreationInfos.builder()
                .equipmentId("idBattery2Edited")
                .equipmentName("nameBatteryModified")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .minActivePower(101.0)
                .maxActivePower(601.0)
                .activePowerSetpoint(401.)
                .reactivePowerSetpoint(51.)
                .minimumReactivePower(23.0)
                .maximumReactivePower(26.0)
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
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getBattery("idBattery1"));
        assertEquals(0, getNetwork().getVoltageLevel("v2").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery1")).count());
    }

    @Test
    public void testCreateWithErrors() throws Exception {
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
        batteryCreationInfos.setMinActivePower(Double.NaN);
        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Battery 'idBattery1': invalid value (NaN) for minimum P",
                batteryCreationInfos.getErrorType().name(), reportService);

        // invalid min max reactive limit
        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setReactiveCapabilityCurve(false);
        batteryCreationInfos.setMinimumReactivePower(Double.NaN);

        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery 'idBattery1' : minimum reactive power is not set").getMessage(),
            batteryCreationInfos.getErrorType().name(), reportService);

        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setReactiveCapabilityCurve(false);
        batteryCreationInfos.setMaximumReactivePower(Double.NaN);

        batteryCreationInfosJson = mapper.writeValueAsString(batteryCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(batteryCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery 'idBattery1' : maximum reactive power is not set").getMessage(),
            batteryCreationInfos.getErrorType().name(), reportService);

        batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setReactiveCapabilityCurve(false);
        batteryCreationInfos.setMinimumReactivePower(200.);
        batteryCreationInfos.setMaximumReactivePower(100.);

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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BATTERY_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"idBattery1\"}");
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BATTERY_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"idBattery2Edited\"}");
    }
}
