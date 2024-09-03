/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Terminal;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("IntegrationTest")
public class OperatingStatusModificationSwitchOnLineTest extends AbstractNetworkModificationTest {

    private static final String TARGET_LINE_ID = "line2";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a disconnection for all terminals (must be reconnected after testCreate)
        Line line = network.getLine(TARGET_LINE_ID);
        assertNotNull(line);
        line.getTerminals().stream().forEach(Terminal::disconnect);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .active(true)
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return OperatingStatusModificationInfos.builder()
                .equipmentId("line1Edited")
                .energizedVoltageLevelId("energizedVoltageLevelIdEdited")
                .stashed(false)
                .active(true)
                .action(OperatingStatusModificationInfos.ActionType.TRIP).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        // terminals are now all connected
        Line line = getNetwork().getLine(TARGET_LINE_ID);
        assertNotNull(line);
        assertTrue(line.getTerminals().stream().allMatch(Terminal::isConnected));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // back to init state : no more connected terminals
        Line line = getNetwork().getLine(TARGET_LINE_ID);
        assertNotNull(line);
        assertTrue(line.getTerminals().stream().noneMatch(Terminal::isConnected));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("SWITCH_ON", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelIdEdited", updatedValues.get("energizedVoltageLevelId"));
        assertEquals("TRIP", updatedValues.get("action"));
        assertEquals("line1Edited", updatedValues.get("equipmentId"));
    }
}
