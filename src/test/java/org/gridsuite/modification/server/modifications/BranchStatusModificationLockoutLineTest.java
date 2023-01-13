/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class BranchStatusModificationLockoutLineTest extends AbstractNetworkModificationTest {

    private static final String targetLineId = "line2";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from PLANNED_OUTAGE (expected after testCreate)
        Line line = network.getLine(targetLineId);
        assertNotNull(line);
        line.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS_MODIFICATION)
                .equipmentId(targetLineId)
                .action(BranchStatusModificationInfos.ActionType.LOCKOUT).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS_MODIFICATION)
                .equipmentId("line1")
                .action(BranchStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        // excepted result of LOCKOUT => extension line.branchStatus == 'PLANNED_OUTAGE'
        assertTrue(TestUtils.checkBranchStatus(getNetwork(), targetLineId, BranchStatus.Status.PLANNED_OUTAGE));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        // go back to init status
        assertTrue(TestUtils.checkBranchStatus(getNetwork(), targetLineId, BranchStatus.Status.FORCED_OUTAGE));
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // line not existing
        BranchStatusModificationInfos modificationInfos = (BranchStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId("notFound");
        String modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().isNotFound(),
                        content().string(new NetworkModificationException(LINE_NOT_FOUND, "notFound").getMessage())
                );

        // modification action empty
        modificationInfos.setEquipmentId("line2");
        modificationInfos.setAction(null);
        modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY).getMessage())
                );

        // modification action not existing
        modificationJson = modificationJson.replace("LOCKOUT", "INVALID_ACTION"); // note: should never happen in real
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(
                        status().is4xxClientError());

        // disconnection error
        modificationInfos.setEquipmentId("line3");
        modificationInfos.setAction(BranchStatusModificationInfos.ActionType.LOCKOUT);
        modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().is5xxServerError(),
                        content().string(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends").getMessage())
                );
    }
}
