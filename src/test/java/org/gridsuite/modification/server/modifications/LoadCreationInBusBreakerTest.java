/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

@Tag("IntegrationTest")
public class LoadCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadCreationInfos.builder()
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
    protected ModificationInfos buildModificationWithOnlyMetadata() {
        ModificationInfos builtModificationInfos = buildModification();
        builtModificationInfos.setStashed(false);
        builtModificationInfos.setMessageType("LOAD_CREATION");
        builtModificationInfos.setMessageValues("{\"equipmentId\":\"idLoad1\"}");
        return builtModificationInfos;
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadCreationInfos.builder()
            .equipmentId("idLoadEdited1")
            .equipmentName("nameLoadEdited1")
            .voltageLevelId("v1Edited")
            .busOrBusbarSectionId("bus1Edited")
            .loadType(LoadType.FICTITIOUS)
            .activePower(300.0)
            .reactivePower(50.0)
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .stashed(false)
            .messageType("LOAD_CREATION")
            .messageValues("{\"equipmentId\":\"idLoadEdited1\"}")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLoad("idLoad1"));
    }
}
