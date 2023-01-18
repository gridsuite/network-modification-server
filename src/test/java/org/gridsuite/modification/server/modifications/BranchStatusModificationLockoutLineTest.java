/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BranchStatus;
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

import static com.powsybl.iidm.network.extensions.BranchStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.BranchStatus.Status.PLANNED_OUTAGE;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class BranchStatusModificationLockoutLineTest extends AbstractNetworkModificationTest {

    private static final String TARGET_LINE_ID = "line2";
    private static final String UPDATE_BRANCH_ID = "line1";
    private static final BranchStatus.Status TARGET_BRANCH_STATUS = PLANNED_OUTAGE;
    private static final BranchStatus.Status OTHER_BRANCH_STATUS = FORCED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from the expected one, after testCreate
        TestUtils.setBranchStatus(network, TARGET_LINE_ID, OTHER_BRANCH_STATUS);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS_MODIFICATION)
                .equipmentId(TARGET_LINE_ID)
                .action(BranchStatusModificationInfos.ActionType.LOCKOUT).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS_MODIFICATION)
                .equipmentId(UPDATE_BRANCH_ID)
                .action(BranchStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        TestUtils.assertBranchStatus(getNetwork(), TARGET_LINE_ID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        // go back to init status
        TestUtils.assertBranchStatus(getNetwork(), TARGET_LINE_ID, OTHER_BRANCH_STATUS);
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
                    content().string(new NetworkModificationException(BRANCH_NOT_FOUND, "notFound").getMessage())
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
                    content().string(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect all branch ends").getMessage())
            );
    }
}
