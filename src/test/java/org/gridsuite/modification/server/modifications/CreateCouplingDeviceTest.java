/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.dto.CreateCouplingDeviceInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
class CreateCouplingDeviceTest extends AbstractNetworkModificationTest {

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
        network.getVoltageLevel("vl1")
            .getNodeBreakerView()
            .newBusbarSection()
            .setId("b9")
            .setName("b9")
            .setNode(20)
            .add();
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return CreateCouplingDeviceInfos.builder()
            .busOrBbsId1("b1")
            .busOrBbsId2("b3")
            .voltageLevelId("v1")
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return CreateCouplingDeviceInfos.builder()
            .busOrBbsId1("b1")
            .busOrBbsId2("b4")
            .voltageLevelId("v1")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertTrue(getNetwork().getSwitchStream().map(Switch::getId).collect(Collectors.toSet())
            .containsAll(Set.of("vl1_BREAKER", "vl1_DISCONNECTOR_22_15", "vl1_DISCONNECTOR_21_0")));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(4, getNetwork().getSwitchStream().map(Switch::getId).collect(Collectors.toSet()).size());
        assertTrue(getNetwork().getSwitchStream().map(Switch::getId).collect(Collectors.toSet()).containsAll(Set.of("br21", "b4", "b5", "br11")));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("{\"voltageLevelId\":\"v1\"}", modificationInfos.getMessageValues());
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("{\"voltageLevelId\":\"v1\"}", modificationInfos.getMessageValues());
    }
}
