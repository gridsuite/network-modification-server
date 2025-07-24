/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class CreateVoltageLevelTopologyTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createSwitchNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected ModificationInfos buildModification() {
        return CreateVoltageLevelTopologyInfos.builder().voltageLevelId("vl1").sectionCount(3)
            .switchKinds(List.of(SwitchKind.DISCONNECTOR, SwitchKind.DISCONNECTOR)).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return CreateVoltageLevelTopologyInfos.builder().voltageLevelId("vl1").sectionCount(2)
            .switchKinds(List.of(SwitchKind.DISCONNECTOR)).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        Assertions.assertEquals(5, busBarIds.size());
        Assertions.assertTrue(busBarIds.containsAll(List.of("b1", "b2", "vl1_2_1", "vl1_2_2", "vl1_2_3")));
        Set<String> switchIds = getNetwork().getSwitchStream().map(Switch::getId).collect(Collectors.toSet());
        Assertions.assertTrue(switchIds.containsAll(Set.of("vl1_DISCONNECTOR_4_5", "vl1_DISCONNECTOR_3_4")));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        List<String> busBarIds = new ArrayList<>();
        getNetwork().getBusbarSections().forEach(busbarSection -> busBarIds.add(busbarSection.getId()));
        Assertions.assertEquals(2, busBarIds.size());
        Assertions.assertFalse(busBarIds.containsAll(List.of("v1_1_1", "v1_1_2", "v1_1_3")));
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
