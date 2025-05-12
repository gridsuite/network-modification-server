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
import org.gridsuite.modification.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class ShuntCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final String ERROR_MESSAGE_KEY = "network.modification.server.errorMessage";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
                .stashed(false)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .equipmentId("shuntOneId")
                .equipmentName("hop")
                .maximumSectionCount(10)
                .sectionCount(6)
                .maxSusceptance(0.)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .connectionName("cn")
                .connectionPosition(99)
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
                .maxSusceptance(0.)
                .voltageLevelId("v4")
                .busOrBusbarSectionId("1.A")
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

    @Test
    void testCreateWithError() throws Exception {
        ShuntCompensatorCreationInfos modificationToCreate = (ShuntCompensatorCreationInfos) buildModification();
        // try to create an existing equipment
        modificationToCreate.setEquipmentId("v5shunt");
        assertNotNull(getNetwork().getShuntCompensator("v5shunt"));
        String modificationToCreateJson = getJsonBody(modificationToCreate, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SHUNT_COMPENSATOR_ALREADY_EXISTS, "v5shunt").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithMaximumSectionCountError() throws Exception {
        ShuntCompensatorCreationInfos modificationToCreate = (ShuntCompensatorCreationInfos) buildModification();
        modificationToCreate.setMaximumSectionCount(0);

        String modificationToCreateJson = getJsonBody(modificationToCreate, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, "Maximum section count should be greater or equal to 1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithSectionError() throws Exception {
        ShuntCompensatorCreationInfos modificationToCreate = (ShuntCompensatorCreationInfos) buildModification();
        modificationToCreate.setMaximumSectionCount(2);
        modificationToCreate.setSectionCount(3);

        String modificationToCreateJson = getJsonBody(modificationToCreate, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, "Section count should be between 0 and Maximum section count (2), actual : 3").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithExistingConnectionPosition() throws Exception {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setConnectionPosition(2);
        String modificationToCreateJson = getJsonBody(dto, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CONNECTION_POSITION_ERROR, "PositionOrder '2' already taken").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithQAtNominalV() throws Exception {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setMaxSusceptance(null);
        dto.setMaxQAtNominalV(80.0);
        //CAPACITOR test
        dto.setShuntCompensatorType(ShuntCompensatorType.CAPACITOR);
        String modificationToCreateJson = getJsonBody(dto, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        ShuntCompensatorCreationInfos createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(dto);
        //REACTOR test
        dto.setShuntCompensatorType(ShuntCompensatorType.REACTOR);
        dto.setEquipmentId("shuntTwoId");
        dto.setConnectionPosition(10);
        modificationToCreateJson = getJsonBody(dto, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);
        assertThat(createdModification).recursivelyEquals(dto);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneId", updatedValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("shuntOneIdEdited", updatedValues.get("equipmentId"));
    }
}
