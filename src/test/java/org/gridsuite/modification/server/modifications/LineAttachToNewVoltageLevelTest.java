/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Tag("IntegrationTest")
class LineAttachToNewVoltageLevelTest extends AbstractNetworkModificationTest {
    private static LineCreationInfos getAttachmentLine() {
        return LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("attachmentLine")
                .r(50.6)
                .x(25.3)
                .build();
    }

    private static VoltageLevelCreationInfos getNewVoltageLevel() {
        return VoltageLevelCreationInfos.builder()
                .stashed(false)
                .equipmentId("newVoltageLevel")
                .equipmentName("NewVoltageLevel")
                .nominalV(379.3)
                .substationId("s1")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(List.of(SwitchKind.BREAKER))
                .couplingDevices(Collections.singletonList(CouplingDeviceInfos.builder().busbarSectionId1("1A").busbarSectionId2("1B").build()))
                .build();
    }

    private static VoltageLevelCreationInfos getAttachmentPoint() {
        return VoltageLevelCreationInfos.builder()
                .equipmentId("AttPointId")
                .stashed(false)
                .nominalV(0)
                .substationCreation(SubstationCreationInfos.builder().stashed(false)
                        .equipmentId("attachmentPointSubstation")
                        .equipmentName("attachmentPointSubstationName")
                        .country(Country.FR)
                        .properties(List.of(FreePropertyInfos.builder()
                                .added(true)
                                .name("substationProp")
                                .value("valueSubstation")
                                .build()))
                        .build())
                .lowVoltageLimit(50.0)
                .highVoltageLimit(100.0)
                .ipMin(5.0)
                .ipMax(20.0)
                .busbarCount(1)
                .sectionCount(1)
                .properties(List.of(FreePropertyInfos.builder()
                        .added(true)
                        .name("voltageLevelProp")
                        .value("valueVoltageLevel")
                        .build()))
                .switchKinds(Collections.emptyList())
                .couplingDevices(Collections.emptyList())
                .build();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineAttachToVoltageLevelInfos.builder()
                .stashed(false)
                .lineToAttachToId("line3")
                .percent(20.0)
                .attachmentPointId("AttPointId")
                .attachmentPointName("attPointName")
                .attachmentPointDetailInformation(getAttachmentPoint())
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
                .lineToAttachToId("line3Edited")
                .stashed(false)
                .percent(10.0)
                .attachmentPointId("AttPointId")   // created VL
                .attachmentPointName("attPointName")
                .attachmentPointDetailInformation(getAttachmentPoint())
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_ATTACH_TO_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line3", createdValues.get("lineToAttachToId"));
    }

}
