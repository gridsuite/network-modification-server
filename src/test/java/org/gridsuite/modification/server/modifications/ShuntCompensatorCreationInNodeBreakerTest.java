/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.utils.MatcherShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CONNECTION_POSITION_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.SHUNT_COMPENSATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ShuntCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
                .date(ZonedDateTime.now())
                .equipmentId("shuntOneId")
                .equipmentName("hop")
                .currentNumberOfSections(4)
                .maximumNumberOfSections(9)
                .susceptancePerSection(1.)
                .isIdenticalSection(true)
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
                .date(ZonedDateTime.now())
                .equipmentId("shuntOneIdEdited")
                .equipmentName("hopEdited")
                .currentNumberOfSections(6)
                .maximumNumberOfSections(12)
                .susceptancePerSection(1.)
                .isIdenticalSection(false)
                .voltageLevelId("v4")
                .busOrBusbarSectionId("1.A")
                .connectionName("cnEdited")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

    @Override
    protected MatcherShuntCompensatorCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherShuntCompensatorCreationInfos.createMatcher((ShuntCompensatorCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @SneakyThrows
    @Test
    public void testCreateWithError() {
        ShuntCompensatorCreationInfos modificationToCreate = (ShuntCompensatorCreationInfos) buildModification();
        // Current number of sections above maximum allowed
        modificationToCreate.setIsIdenticalSection(false);
        modificationToCreate.setCurrentNumberOfSections(6);
        modificationToCreate.setMaximumNumberOfSections(2);
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNull(getNetwork().getShuntCompensator(modificationToCreate.getEquipmentId()));

        // try to create an existing equipment
        modificationToCreate.setEquipmentId("v5shunt");
        assertNotNull(getNetwork().getShuntCompensator("v5shunt"));
        modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SHUNT_COMPENSATOR_ALREADY_EXISTS, "v5shunt").getMessage(),
                modificationToCreate.getErrorType().name(), reportService);
    }

    @SneakyThrows
    @Test
    public void testCreateWithExistingConnectionPosition() {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setConnectionPosition(2);
        String modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CONNECTION_POSITION_ERROR, "PositionOrder '2' already taken").getMessage(),
                dto.getErrorType().name(), reportService);
    }

    @SneakyThrows
    @Test
    public void testCreateWithQAtNominalV() {
        ShuntCompensatorCreationInfos dto = (ShuntCompensatorCreationInfos) buildModification();
        dto.setSusceptancePerSection(null);
        dto.setQAtNominalV(80.0);
        //CAPACITOR test
        dto.setShuntCompensatorType(ShuntCompensatorType.CAPACITOR);
        String modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        ShuntCompensatorCreationInfos createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification, createMatcher(dto));
        //REACTOR test
        dto.setShuntCompensatorType(ShuntCompensatorType.REACTOR);
        dto.setEquipmentId("shuntTwoId");
        dto.setConnectionPosition(10);
        modificationToCreateJson = mapper.writeValueAsString(dto);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);
        assertThat(createdModification, createMatcher(dto));
    }
}
