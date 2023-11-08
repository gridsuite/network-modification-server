/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BranchStatus;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.BranchStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.BranchStatus.Status.PLANNED_OUTAGE;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("IntegrationTest")
public class BranchStatusModificationTripLineTest extends AbstractNetworkModificationTest {

    private static final String TARGET_LINE_ID = "line2";
    private static final String UPDATE_BRANCH_ID = "line1";
    private static final BranchStatus.Status TARGET_BRANCH_STATUS = FORCED_OUTAGE;
    private static final BranchStatus.Status OTHER_BRANCH_STATUS = PLANNED_OUTAGE;

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
                .stashed(false)
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(BranchStatusModificationInfos.ActionType.TRIP).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BranchStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(UPDATE_BRANCH_ID)
                .energizedVoltageLevelId("energizedVoltageLevelIdEdited")
                .action(BranchStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TestUtils.assertBranchStatus(getNetwork(), TARGET_LINE_ID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // back to init status
        TestUtils.assertBranchStatus(getNetwork(), TARGET_LINE_ID, OTHER_BRANCH_STATUS);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BRANCH_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("TRIP", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BRANCH_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelIdEdited", updatedValues.get("energizedVoltageLevelId"));
        assertEquals("SWITCH_ON", updatedValues.get("action"));
        assertEquals("line1", updatedValues.get("equipmentId"));
    }
}
