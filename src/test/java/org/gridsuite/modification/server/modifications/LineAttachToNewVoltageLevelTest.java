/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LineAttachToNewVoltageLevelTest extends AbstractNetworkModificationTest {

    private LineCreationInfos getAttachmentLine() {
        return LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId("attachmentLine")
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();
    }

    private VoltageLevelCreationInfos getNewVoltageLevel() {
        return VoltageLevelCreationInfos.builder()
                .type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .equipmentId("newVoltageLevel")
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
                .busbarConnections(List.of())
                .build();
    }

    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line3")
                .percent(20.0)
                .attachmentPointId("AttPointId")
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())  // create another new VL
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line3")
                .percent(10.0)
                .attachmentPointId("AttPointId")   // created VL
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("v4")     // use existing VL
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())   // created Line
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherLineAttachToVoltageLevelInfos((LineAttachToVoltageLevelInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        // new equipments in the network:
        assertNotNull(getNetwork().getLine("attachmentLine"));
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
        assertNotNull(getNetwork().getVoltageLevel("AttPointId"));
        assertNotNull(getNetwork().getVoltageLevel("newVoltageLevel"));
        // replaced line is gone
        assertNull(getNetwork().getLine("line3"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLine("attachmentLine"));
        assertNull(getNetwork().getLine("nl1"));
        assertNull(getNetwork().getLine("nl2"));
        assertNull(getNetwork().getVoltageLevel("AttPointId"));
        assertNull(getNetwork().getVoltageLevel("newVoltageLevel"));
        assertNotNull(getNetwork().getLine("line3"));
    }
}
