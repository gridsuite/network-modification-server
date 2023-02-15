/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.ConnectablePositionInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLineCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.UUID;

import static org.gridsuite.modification.server.utils.MatcherLineCreationInfos.createMatcherLineCreationInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LineCreationInMixedTypologyTest extends AbstractNetworkModificationTest {

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
            .id("idLine1")
            .name("nameLine1")
            .r(100.0)
            .x(100.0)
            .shuntConductance1(10.0)
            .shuntSusceptance1(10.0)
            .shuntConductance2(20.0)
            .shuntSusceptance2(20.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .position1(ConnectablePositionInfos.builder()
                .label("cn1Line1")
                .direction(ConnectablePosition.Direction.TOP)
                .order(0).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn2Line1")
                .direction(ConnectablePosition.Direction.TOP)
                .order(0).build())
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
            .id("idLineEdited1")
            .name("nameLineEdited1")
            .r(200.0)
            .x(200.0)
            .shuntConductance1(20.0)
            .shuntSusceptance1(20.0)
            .shuntConductance2(30.0)
            .shuntSusceptance2(20.0)
            .voltageLevelId1("v1Edited")
            .busOrBusbarSectionId1("2.1")
            .voltageLevelId2("v3")
            .busOrBusbarSectionId2("bus3")
            .position1(ConnectablePositionInfos.builder()
                .label("cn3Line1")
                .direction(ConnectablePosition.Direction.BOTTOM)
                .order(0).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn4Line1")
                .direction(ConnectablePosition.Direction.BOTTOM)
                .order(0).build())
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
