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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class ShuntCompensatorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Test
    void testCreateWithErrors() throws Exception {
        ShuntCompensatorCreationInfos shunt = (ShuntCompensatorCreationInfos) buildModification();
        shunt.setBusOrBusbarSectionId("notFoundBus");
        String shuntJson = getJsonBody(shunt, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
            .stashed(false)
            .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
            .equipmentId("shuntOneId")
            .equipmentName("hopOne")
            .maximumSectionCount(10)
            .sectionCount(6)
            .maxSusceptance(0.)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("bus2")
            .connectionName("cn2")
            .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorCreationInfos.builder()
                .stashed(false)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
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
        assertEquals(PROPERTY_VALUE, getNetwork().getShuntCompensator("shuntOneId").getProperty(PROPERTY_NAME));

    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneId", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneIdEdited", updatedValues.get("equipmentId"));
    }
}
