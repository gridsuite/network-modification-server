/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;

import lombok.SneakyThrows;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import static org.gridsuite.modification.server.NetworkModificationException.Type.DELETE_EQUIPMENT_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class EquipmentDeletionTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v1load")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("GENERATOR")
                .equipmentId("idGenerator")
                .build();
    }

    @Override
    protected MatcherEquipmentDeletionInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos((EquipmentDeletionInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getLoad("v1load"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLoad("v1load"));
    }

    @SneakyThrows
    @Test
    public void testOkWhenRemovingIsolatedEquipment() {

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v5load")
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        var v5 = getNetwork().getVoltageLevel("v5");
        assertNull(v5.getNodeBreakerView().getTerminal(2));
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // delete load (fail because the load is not found)
        EquipmentDeletionInfos equipmentDeletionInfos = (EquipmentDeletionInfos) buildModification();
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        mockMvc
                .perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isNotFound(),
                        content().string(new NetworkModificationException(EQUIPMENT_NOT_FOUND,
                                "Equipment with id=notFoundLoad not found or of bad type").getMessage()));

        // try to delete voltage level (Internal error because the vl is still connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().is5xxServerError(),
                        content().string(new NetworkModificationException(DELETE_EQUIPMENT_ERROR,
                            new PowsyblException(new AssertionError("The voltage level 'v4' cannot be removed because of a remaining THREE_WINDINGS_TRANSFORMER"))).getMessage()));
        equipmentDeletionInfos.setEquipmentId("v4");
        assertNotNull(getNetwork().getVoltageLevel("v4"));

        // try to delete substation (Internal error because the substation is still connected)
        equipmentDeletionInfos.setEquipmentType("SUBSTATION");
        equipmentDeletionInfos.setEquipmentId("s2");
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().is5xxServerError(),
                        content().string(new NetworkModificationException(DELETE_EQUIPMENT_ERROR,
                                "The substation s2 is still connected to another substation").getMessage()));
        assertNotNull(getNetwork().getSubstation("s2"));
    }
}
