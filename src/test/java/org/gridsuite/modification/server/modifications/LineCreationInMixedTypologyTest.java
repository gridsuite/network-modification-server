/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.FreePropertyInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("IntegrationTest")
public class LineCreationInMixedTypologyTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createMixedTopology(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new line in voltage levels with node breaker topology and bus breaker topology
        // between voltage level "v1" and busbar section "1.1" type NODE_BREAKER and
        //         voltage level "v2" and busbar section "bus2 type BUS_BREAKER"
        return LineCreationInfos.builder()
            .stashed(false)
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .r(100.0)
            .x(100.0)
            .g1(10.0)
            .b1(10.0)
            .g2(20.0)
            .b2(20.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .connectionName1("cn1Line1")
            .connectionDirection1(ConnectablePosition.Direction.TOP)
            .connectionName2("cn2Line1")
            .connectionDirection2(ConnectablePosition.Direction.TOP)
            .connectionPosition1(0)
            .connectionPosition2(0)
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
            .stashed(false)
            .equipmentId("idLineEdited1")
            .equipmentName("nameLineEdited1")
            .r(200.0)
            .x(200.0)
            .g1(20.0)
            .b1(20.0)
            .g2(30.0)
            .b2(20.0)
            .voltageLevelId1("v1Edited")
            .busOrBusbarSectionId1("2.1")
            .voltageLevelId2("v3")
            .busOrBusbarSectionId2("bus3")
            .connectionName1("cn3Line1")
            .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
            .connectionName2("cn4Line1")
            .connectionDirection2(ConnectablePosition.Direction.BOTTOM)
            .connectionPosition1(0)
            .connectionPosition2(0)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLine("idLine1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLine("idLine1").getProperty(PROPERTY_NAME));

    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLine("idLine1"));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLine1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLineEdited1", updatedValues.get("equipmentId"));
    }
}
