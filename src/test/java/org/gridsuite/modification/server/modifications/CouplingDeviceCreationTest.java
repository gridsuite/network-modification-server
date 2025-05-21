/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.dto.CouplingDeviceCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.UUID;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
class CouplingDeviceCreationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.createSwitchNetwork(networkUuid, new NetworkFactoryImpl());
        network.getVoltageLevel("vl1")
            .getNodeBreakerView()
            .newBusbarSection()
            .setId("b3")
            .setName("b3")
            .setNode(15)
            .add();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return CouplingDeviceCreationInfos.builder()
            .busOrBbsId1("b1")
            .busOrBbsId2("b3")
            .switchPrefixId("test")
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return null;
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        System.out.println("After network modification creation");
        getNetwork().getSwitches().forEach(switchId -> {System.out.println(switchId.getId());});
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        System.out.println("After network modification deletion");
        getNetwork().getSwitches().forEach(switchId -> {System.out.println(switchId.getId());});
    }
}
