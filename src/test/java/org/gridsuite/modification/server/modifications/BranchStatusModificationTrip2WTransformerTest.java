/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BranchStatus;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;

import java.util.UUID;

import static com.powsybl.iidm.network.extensions.BranchStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.BranchStatus.Status.PLANNED_OUTAGE;
import static org.gridsuite.modification.server.utils.MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos;

public class BranchStatusModificationTrip2WTransformerTest extends AbstractNetworkModificationTest {

    private static final String TARGET_BRANCH_ID = "trf1";
    private static final String UPDATE_BRANCH_ID = "line1"; // it is not a 2WT, but is does not matter
    private static final BranchStatus.Status TARGET_BRANCH_STATUS = FORCED_OUTAGE;
    private static final BranchStatus.Status OTHER_BRANCH_STATUS = PLANNED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from the expected one, after testCreate
        TestUtils.setBranchStatus(network, TARGET_BRANCH_ID, OTHER_BRANCH_STATUS);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS_MODIFICATION)
                .equipmentId(TARGET_BRANCH_ID)
                .action(BranchStatusModificationInfos.ActionType.TRIP).build();
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
        TestUtils.assertBranchStatus(getNetwork(), TARGET_BRANCH_ID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        // back to init status
        TestUtils.assertBranchStatus(getNetwork(), TARGET_BRANCH_ID, OTHER_BRANCH_STATUS);
    }
}
