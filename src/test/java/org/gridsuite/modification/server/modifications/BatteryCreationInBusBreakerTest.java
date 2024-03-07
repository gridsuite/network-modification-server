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
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.dto.FreePropertyInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class BatteryCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId("idBattery2")
                .equipmentName("nameBattery2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
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
        assertNotNull(getNetwork().getBattery("idBattery2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery2")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("idBattery2").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getBattery("idBattery2"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery2")).count());
    }

    @Test
    public void testCreateWithBusbarSectionErrors() throws Exception {
        BatteryCreationInfos batteryCreationInfos = (BatteryCreationInfos) buildModification();
        batteryCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(batteryCreationInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                batteryCreationInfos.getErrorType().name(), reportService);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BATTERY_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery2", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BATTERY_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery2Edited", updatedValues.get("equipmentId"));
    }
}
