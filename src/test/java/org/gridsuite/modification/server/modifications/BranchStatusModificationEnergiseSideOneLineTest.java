/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_ERROR;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class BranchStatusModificationEnergiseSideOneLineTest extends AbstractNetworkModificationTest {

    private static final String TARGET_LINE_ID = "line2";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a disconnection for terminal1, a connection for terminal2 (must be reconnected/disconnected after testCreate)
        Line line = network.getLine(TARGET_LINE_ID);
        assertNotNull(line);
        line.getTerminal1().disconnect();
        line.getTerminal2().connect();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return BranchStatusModificationInfos.builder()
                .equipmentId(TARGET_LINE_ID)
                .action(BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BranchStatusModificationInfos.builder()
                .equipmentId("line1")
                .action(BranchStatusModificationInfos.ActionType.TRIP).build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
        // terminal1 is now connected, terminal2 is now disconnected
        Line line = getNetwork().getLine(TARGET_LINE_ID);
        assertNotNull(line);
        assertTrue(line.getTerminal1().isConnected());
        assertFalse(line.getTerminal2().isConnected());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        // back to init state
        Line line = getNetwork().getLine(TARGET_LINE_ID);
        assertNotNull(line);
        assertFalse(line.getTerminal1().isConnected());
        assertTrue(line.getTerminal2().isConnected());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // line not existing
        BranchStatusModificationInfos modificationInfos = (BranchStatusModificationInfos) buildModification();
        // disconnection error
        modificationInfos.setEquipmentId("line3");
        String modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise branch end").getMessage(),
                modificationInfos.getErrorType().name(), reportService);
    }
}
