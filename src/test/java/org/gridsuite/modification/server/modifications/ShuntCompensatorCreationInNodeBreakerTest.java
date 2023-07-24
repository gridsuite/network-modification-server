/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CONNECTION_POSITION_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.SHUNT_COMPENSATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.assertions.Assertions.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class ShuntCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
                .date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .equipmentId("shuntOneId")
                .equipmentName("hop")
                .maximumNumberOfSections(1)
                .susceptancePerSection(0.)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .connectionName("cn")
                .connectionPosition(99)
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorCreationInfos.builder()
                .date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .equipmentId("shuntOneIdEdited")
                .equipmentName("hopEdited")
                .maximumNumberOfSections(1)
                .susceptancePerSection(0.)
                .voltageLevelId("v4")
                .busOrBusbarSectionId("1.A")
                .connectionName("cnEdited")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @Test
    public void testCreateWithError() throws Exception {
        ShuntCompensatorCreationInfos modificationToCreate = (ShuntCompensatorCreationInfos) buildModification();
        // try to create an existing equipment
        modificationToCreate.setEquipmentId("v5shunt");
        assertNotNull(getNetwork().getShuntCompensator("v5shunt"));
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SHUNT_COMPENSATOR_ALREADY_EXISTS, "v5shunt").getMessage(),
                modificationToCreate.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateWithExistingConnectionPosition() throws Exception {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setConnectionPosition(2);
        String modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CONNECTION_POSITION_ERROR, "PositionOrder '2' already taken").getMessage(),
                dto.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateWithQAtNominalV() throws Exception {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setSusceptancePerSection(null);
        dto.setQAtNominalV(80.0);
        //CAPACITOR test
        dto.setShuntCompensatorType(ShuntCompensatorType.CAPACITOR);
        String modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        ShuntCompensatorCreationInfos createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(dto);
        //REACTOR test
        dto.setShuntCompensatorType(ShuntCompensatorType.REACTOR);
        dto.setEquipmentId("shuntTwoId");
        dto.setConnectionPosition(10);
        modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);
        assertThat(createdModification).recursivelyEquals(dto);
    }

    @Test
    public void testCreateToAssignSectionCount() throws Exception {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setQAtNominalV(0.);
        String modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        ShuntCompensatorCreationInfos createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertNull(createdModification.getCurrentNumberOfSections());
        modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertNull(createdModification.getCurrentNumberOfSections());
    }
}
