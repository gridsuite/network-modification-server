/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Terminal;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("IntegrationTest")
public class BranchStatusModificationSwitchOnLineTest extends AbstractNetworkModificationTest {

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
        return BranchStatusModificationInfos.builder()
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(BranchStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BranchStatusModificationInfos.builder()
                .equipmentId("line1Edited")
                .energizedVoltageLevelId("energizedVoltageLevelIdEdited")
                .action(BranchStatusModificationInfos.ActionType.TRIP).build();
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BRANCH_STATUS_MODIFICATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"energizedVoltageLevelId\":\"energizedVoltageLevelId\",\"action\":\"SWITCH_ON\",\"equipmentId\":\"line2\"}");
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BRANCH_STATUS_MODIFICATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"energizedVoltageLevelId\":\"energizedVoltageLevelIdEdited\",\"action\":\"TRIP\",\"equipmentId\":\"line1Edited\"}");
    }
}
