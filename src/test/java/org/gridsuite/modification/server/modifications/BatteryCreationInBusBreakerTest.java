/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class BatteryCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return BatteryCreationInfos.builder()
                .equipmentId("idBattery2")
                .equipmentName("nameBattery2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
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
        assertNotNull(getNetwork().getBattery("idBattery2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getBatteryStream()
                .filter(transformer -> transformer.getId().equals("idBattery2")).count());
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BATTERY_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"idBattery2\"}");
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BATTERY_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"idBattery2Edited\"}");
    }
}
