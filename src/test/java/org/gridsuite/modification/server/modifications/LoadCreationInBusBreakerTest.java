/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition.Direction;

import org.gridsuite.modification.server.dto.ConnectablePositionInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLoadCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LoadCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadCreationInfos.builder()
            .id("idLoad1")
            .name("nameLoad1")
            .voltageLevelId("v1")
            .busOrBusbarSectionId("bus1")
            .loadType(LoadType.FICTITIOUS)
            .p0(200.0)
            .q0(30.0)
            .position(ConnectablePositionInfos.builder()
                .label("top")
                .direction(Direction.TOP)
                .build())
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadCreationInfos.builder()
            .id("idLoadEdited1")
            .name("nameLoadEdited1")
            .voltageLevelId("v1Edited")
            .busOrBusbarSectionId("bus1Edited")
            .loadType(LoadType.FICTITIOUS)
            .p0(300.0)
            .q0(50.0)
            .position(ConnectablePositionInfos.builder()
                .label("bottom")
                .direction(Direction.BOTTOM)
                .build())
            .build();
    }

    @Override
    protected MatcherLoadCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadCreationInfos.createMatcherLoadCreationInfos((LoadCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLoad("idLoad1"));
    }
}
