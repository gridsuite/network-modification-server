/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLineCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import static org.gridsuite.modification.server.utils.MatcherLineCreationInfos.createMatcherLineCreationInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LineCreationInMixedTypologyTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork() {
        return NetworkCreation.createMixedTopology(TEST_NETWORK_ID);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new line in voltage levels with node breaker topology and bus breaker topology
        // between voltage level "v1" and busbar section "1.1" type NODE_BREAKER and
        //         voltage level "v2" and busbar section "bus2 type BUS_BREAKER"
        return LineCreationInfos.builder()
            .type(ModificationType.LINE_CREATION)
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .seriesResistance(100.0)
            .seriesReactance(100.0)
            .shuntConductance1(10.0)
            .shuntSusceptance1(10.0)
            .shuntConductance2(20.0)
            .shuntSusceptance2(20.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .connectionName1("cn1Line1")
            .connectionDirection1(ConnectablePosition.Direction.TOP)
            .connectionName2("cn2Line1")
            .connectionDirection2(ConnectablePosition.Direction.TOP)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
            .type(ModificationType.LINE_CREATION)
            .equipmentId("idLineEdited1")
            .equipmentName("nameLineEdited1")
            .seriesResistance(200.0)
            .seriesReactance(200.0)
            .shuntConductance1(20.0)
            .shuntSusceptance1(20.0)
            .shuntConductance2(30.0)
            .shuntSusceptance2(20.0)
            .voltageLevelId1("v1Edited")
            .busOrBusbarSectionId1("2.1")
            .voltageLevelId2("v3")
            .busOrBusbarSectionId2("bus3")
            .connectionName1("cn3Line1")
            .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
            .connectionName2("cn4Line1")
            .connectionDirection2(ConnectablePosition.Direction.BOTTOM)
            .build();
    }

    @Override
    protected MatcherLineCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherLineCreationInfos((LineCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLine("idLine1"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLine("idLine1"));
    }
}
