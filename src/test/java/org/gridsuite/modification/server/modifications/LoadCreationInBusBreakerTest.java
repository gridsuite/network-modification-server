/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLoadCreationInfos;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LoadCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_BUS_BREAKER_ID;
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadCreationInfos.builder()
            .type(ModificationType.LOAD_CREATION)
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v1")
            .busOrBusbarSectionId("bus1")
            .loadType(LoadType.FICTITIOUS)
            .activePower(200.0)
            .reactivePower(30.0)
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadCreationInfos.builder()
            .type(ModificationType.LOAD_CREATION)
            .equipmentId("idLoadEdited1")
            .equipmentName("nameLoadEdited1")
            .voltageLevelId("v1Edited")
            .busOrBusbarSectionId("bus1Edited")
            .loadType(LoadType.FICTITIOUS)
            .activePower(300.0)
            .reactivePower(50.0)
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
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
