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
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;

import java.util.UUID;

import static org.gridsuite.modification.server.utils.MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class BranchStatusModificationTripLineTest extends AbstractNetworkModificationTest {

    private static final String targetLineId = "line2";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from FORCED_OUTAGE (expected after testCreate)
        Line line = network.getLine(targetLineId);
        assertNotNull(line);
        line.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS_MODIFICATION)
                .equipmentId(targetLineId)
                .action(BranchStatusModificationInfos.ActionType.TRIP).build();
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
        assertTrue(TestUtils.checkBranchStatus(getNetwork(), targetLineId, BranchStatus.Status.FORCED_OUTAGE));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertTrue(TestUtils.checkBranchStatus(getNetwork(), targetLineId, BranchStatus.Status.PLANNED_OUTAGE));
    }
}
