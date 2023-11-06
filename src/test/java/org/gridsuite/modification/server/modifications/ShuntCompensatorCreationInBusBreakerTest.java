/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class ShuntCompensatorCreationInBusBreakerTest extends AbstractNetworkModificationTest {

    @Test
    public void testCreateWithErrors() throws Exception {
        ShuntCompensatorCreationInfos shunt = (ShuntCompensatorCreationInfos) buildModification();
        shunt.setBusOrBusbarSectionId("notFoundBus");
        String shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(getNetworkModificationUri()).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                shunt.getErrorType().name(), reportService);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
            .stashed(false)
            .date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
            .equipmentId("shuntOneId")
            .equipmentName("hopOne")
            .maximumSectionCount(10)
            .sectionCount(6)
            .maxSusceptance(0.)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("bus2")
            .connectionName("cn2")
            .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorCreationInfos.builder()
                .stashed(false)
                .date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .equipmentId("shuntOneIdEdited")
                .equipmentName("hopEdited")
                .maximumSectionCount(20)
                .sectionCount(3)
                .maxSusceptance(1.)
                .voltageLevelId("v4")
                .busOrBusbarSectionId("bus3")
                .connectionName("cnEdited")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneId", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneIdEdited", updatedValues.get("equipmentId"));
    }
}
