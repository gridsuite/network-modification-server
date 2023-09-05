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
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
            .date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
            .equipmentId("shuntOneId")
            .equipmentName("hopOne")
            .maximumNumberOfSections(1)
            .susceptancePerSection(0.)
            .voltageLevelId("v2")
            .busOrBusbarSectionId("bus2")
            .connectionName("cn2")
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
                .susceptancePerSection(1.)
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

}
