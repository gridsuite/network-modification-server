/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class VoltageLevelDeletionTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .equipmentType("VOLTAGE_LEVEL")
                .equipmentId("v1")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return null;
    }

    @Override
    protected MatcherEquipmentDeletionInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos((EquipmentDeletionInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getVoltageLevel("v1"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getVoltageLevel("v1"));
    }

    @SneakyThrows
    @Test
    public void testRemoveVoltageLevel() {
        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL.name())
                .equipmentId("v1")
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        assertNull(getNetwork().getVoltageLevel("v1"));
        assertNull(getNetwork().getLoad("v1load"));
        assertNull(getNetwork().getLccConverterStation("v1lcc"));
        assertNull(getNetwork().getSwitch("v1d1"));
        assertNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getHvdcLine("hvdcLine"));
    }
}
