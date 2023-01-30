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
import org.gridsuite.modification.server.utils.MatcherShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_SHUNT_COMPENSATOR_ERROR;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ShuntCompensatorCreationInBusBreakerTest extends AbstractNetworkModificationTest {

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        ShuntCompensatorCreationInfos shunt = (ShuntCompensatorCreationInfos) buildModification();
        shunt.setCurrentNumberOfSections(6);
        shunt.setMaximumNumberOfSections(2);
        String shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(getNetworkModificationUri()).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_SHUNT_COMPENSATOR_ERROR, String.format("Shunt compensator '%s': the current number (%s) of section should be lesser than the maximum number of section (%s)", shunt.getEquipmentId(), 6, 2)).getMessage()));

        shunt.setBusOrBusbarSectionId("notFoundBus");
        shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(getNetworkModificationUri()).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isNotFound(), content().string(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
            .date(ZonedDateTime.now())
            .equipmentId("shuntOneId")
            .equipmentName("hopOne")
            .currentNumberOfSections(4)
            .maximumNumberOfSections(9)
            .susceptancePerSection(1.)
            .isIdenticalSection(true)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("bus2")
            .connectionName("cn2")
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
                .busOrBusbarSectionId("bus3")
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

}
