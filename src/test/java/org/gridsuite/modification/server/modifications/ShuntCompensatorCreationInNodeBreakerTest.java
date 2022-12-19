/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.utils.MatcherShuntCompensatorCreationInfos;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ShuntCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
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
                .andExpect(status().is5xxServerError());
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
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
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
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

}
