/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;

import java.util.UUID;

import static org.gridsuite.modification.server.utils.MatcherDeleteAttachingLineInfos.createMatcherDeleteAttachingLineInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteAttachingLineTest extends AbstractNetworkModificationTest {

    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_DELETE_ATTACHING_LINE_ID;
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteAttachingLineInfos.builder()
                .type(ModificationType.DELETE_ATTACHING_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineId")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return DeleteAttachingLineInfos.builder()
                .type(ModificationType.DELETE_ATTACHING_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineIdEdited")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherDeleteAttachingLineInfos((DeleteAttachingLineInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("replacingLineId"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNotNull(getNetwork().getLine("l2"));
        assertNull(getNetwork().getLine("replacingLineIdEdited"));
    }
}
