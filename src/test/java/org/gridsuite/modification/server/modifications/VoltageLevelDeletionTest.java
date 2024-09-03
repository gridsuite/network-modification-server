/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

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
                .stashed(false)
                .active(true)
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
                .equipmentId("v1")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .stashed(false)
                .active(true)
                .equipmentType(IdentifiableType.LINE)
                .equipmentId("v2")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getVoltageLevel("v1"));
        assertNull(getNetwork().getLoad("v1load"));
        assertNull(getNetwork().getLccConverterStation("v1lcc"));
        assertNull(getNetwork().getSwitch("v1d1"));
        assertNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getHvdcLine("hvdcLine"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getVoltageLevel("v1"));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("EQUIPMENT_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("EQUIPMENT_DELETION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v2", updatedValues.get("equipmentId"));
    }
}
