/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.springframework.test.context.TestPropertySource;

import java.util.Arrays;
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
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ModificationCreation.getCreationVoltageLevel("s2", "vl1", "vlName");
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VoltageLevelCreationInfos.builder()
                .stashed(false)
                .equipmentId("VoltageLevelIdEdited")
                .equipmentName("VoltageLevelEdited")
                .substationId("s2")
                .nominalV(385)
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("1A").busbarSectionId2("1.A").build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Set<String> busbarIds = getNetwork()
                .getBusbarSectionStream()
                .map(BusbarSection::getId)
                .collect(Collectors.toSet());
        assertTrue(busbarIds.containsAll(Set.of("1.1", "1A", "1B", "1.A", "1A1", "3A", "1B1", "BUSBAR_1_1", "BUSBAR_2_1", "BUSBAR_1_2", "BUSBAR_2_2")));
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
