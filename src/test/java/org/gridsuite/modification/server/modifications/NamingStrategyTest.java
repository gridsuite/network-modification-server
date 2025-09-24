/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.dto.CouplingDeviceInfos;
import org.gridsuite.modification.dto.CreateCouplingDeviceInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.springframework.test.context.TestPropertySource;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Tag("IntegrationTest")
@TestPropertySource(locations = "classpath:application-dummy-naming-strategy.yml")
class NamingStrategyTest extends AbstractNetworkModificationTest {

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
                .couplingDeviceInfos(CouplingDeviceInfos.builder()
                        .busbarSectionId1("b1")
                        .busbarSectionId2("b3")
                        .build())
                .voltageLevelId("v1")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return CreateCouplingDeviceInfos.builder()
                .couplingDeviceInfos(CouplingDeviceInfos.builder()
                        .busbarSectionId1("b1")
                        .busbarSectionId2("b4")
                        .build())
                .voltageLevelId("v1")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertTrue(getNetwork().getSwitchStream().map(Switch::getId).collect(Collectors.toSet())
                // verify that the switches names follow the dummy naming strategy
                .containsAll(Set.of("SWITCH_b1_b3", "DISCONNECTOR_22_15_0", "DISCONNECTOR_21_0_0")));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // nothing to test
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        // nothing to test
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        // nothing to test
    }

}
