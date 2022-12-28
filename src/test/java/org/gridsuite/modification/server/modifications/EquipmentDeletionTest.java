/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

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
import org.springframework.web.util.NestedServletException;

import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThrows;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.powsybl.iidm.network.VariantManagerConstants;

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
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1load", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLoad("v1load"));
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

        // delete voltage level (fail because the vl is connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        assertThrows("\"status\":500,\"error\":\"Internal Server Error\",\"message\":\"The voltage level 'v4' cannot be removed because of a remaining THREE_WINDINGS_TRANSFORMER",
                NestedServletException.class, () -> mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos))
                                .contentType(MediaType.APPLICATION_JSON)));
        assertNotNull(getNetwork().getVoltageLevel("v4"));

        // delete substation (fail because the substations is connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        assertThrows("DELETE_EQUIPMENT_ERROR : The substation s2 is still connected to another substation",
                NestedServletException.class, () -> mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos))
                                .contentType(MediaType.APPLICATION_JSON))
                        .andReturn());
        assertNotNull(getNetwork().getSubstation("s2"));
    }
}
