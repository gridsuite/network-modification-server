/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.MatcherShuntCompensatorCreationInfos;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ShuntCompensatorCreationTest extends AbstractNetworkModificationTest {

    public void testCreateWithError() throws Exception {

        ShuntCompensatorCreationInfos modificationToCreate = buildShuntCompensatorCreationInfos();
        // Current number of sections above maximum allowed
        modificationToCreate.setIsIdenticalSection(false);
        modificationToCreate.setCurrentNumberOfSections(6);
        modificationToCreate.setMaximumNumberOfSections(2);
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        mockMvc.perform(post(URI_NETWORK_MODIF).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError());
    }

    // old test moved here
    @Test
    public void testCreateShuntCompensatorInBusBreaker() throws Exception {

        ShuntCompensatorCreationInfos shunt = ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
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

        String shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        ShuntCompensatorCreationInfos bsmlrShuntCompensator = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(bsmlrShuntCompensator, MatcherShuntCompensatorCreationInfos.createMatcher(shunt));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        shunt.setEquipmentId("shuntTwoId");
        shunt.setEquipmentName("hopTwo");
        shunt.setIsIdenticalSection(false);
        shunt.setCurrentNumberOfSections(6);
        shunt.setMaximumNumberOfSections(2);
        shunt.setSusceptancePerSection(2.);
        shunt.setVoltageLevelId("v1");
        shunt.setBusOrBusbarSectionId("bus1");
        shunt.setUuid(bsmlrShuntCompensator.getUuid());
        shuntJson = mapper.writeValueAsString(shunt);
        // it works with maximumNumberOfSections < currentNumberOfSections ?
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        shunt.setBusOrBusbarSectionId("notFoundBus");
        shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage()));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);
    }

    private ShuntCompensatorCreationInfos buildShuntCompensatorCreationInfos() {
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

    private ShuntCompensatorCreationInfos buildShuntCompensatorCreationInfosUpdate() {
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

    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherShuntCompensatorCreationInfos.createMatcher((ShuntCompensatorCreationInfos) modificationInfos);
    }

    protected void assertNetworkAfterCreation() {
        assertNotNull(network.getShuntCompensator("shuntOneId"));
    }

    protected void assertNetworkAfterDeletion() {
        assertNull(network.getShuntCompensator("shuntOneId"));
    }

}
