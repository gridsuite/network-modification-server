/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Rehili Ghazwa <ghazwa.rehili at rte-france.com>
 */
class CreateVoltageLevelSectionTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.createSwitchNetwork(networkUuid, new NetworkFactoryImpl());
        network.getVoltageLevel("vl1").getNodeBreakerView().getBusbarSectionStream().forEach(busbarSection -> {
            if (busbarSection.getExtension(BusbarSectionPosition.class) == null) {
                busbarSection.newExtension(BusbarSectionPositionAdder.class)
                        .withBusbarIndex(1)
                        .withSectionIndex(1)
                        .add();
            }
        });
        BusbarSection b3 = network.getVoltageLevel("vl1")
                .getNodeBreakerView()
                .newBusbarSection()
                .setId("b3")
                .setName("b3")
                .setNode(15)
                .add();
        b3.newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(1)
                .withSectionIndex(1)
                .add();

        BusbarSection b9 = network.getVoltageLevel("vl1")
                .getNodeBreakerView()
                .newBusbarSection()
                .setId("b9")
                .setName("b9")
                .setNode(20)
                .add();
        b9.newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(1)
                .withSectionIndex(1)
                .add();

        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return CreateVoltageLevelSectionInfos.builder()
                .stashed(false)
                .voltageLevelId("vl1")
                .busbarSectionId("b3")
                .busbarIndex(1)
                .isAfterBusbarSectionId(true)
                .leftSwitchKind("BREAKER")
                .rightSwitchKind("DISCONNECTOR")
                .isAllBusbars(false)
                .isSwitchOpen(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return CreateVoltageLevelSectionInfos.builder()
                .voltageLevelId("vl1")
                .busbarSectionId("b3")
                .busbarIndex(1)
                .isAfterBusbarSectionId(true)
                .leftSwitchKind("DISCONNECTOR")
                .rightSwitchKind("DISCONNECTOR")
                .isAllBusbars(false)
                .isSwitchOpen(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        assertTrue(busBarIds.size() > 4);
        assertTrue(busBarIds.containsAll(List.of("b1", "b2", "b3", "b9", "BUSBAR_1_2")));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        assertEquals(4, busBarIds.size());
        assertTrue(busBarIds.containsAll(List.of("b1", "b2", "b3", "b9")));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("{\"voltageLevelId\":\"vl1\"}", modificationInfos.getMessageValues());
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("{\"voltageLevelId\":\"vl1\"}", modificationInfos.getMessageValues());
    }
}
