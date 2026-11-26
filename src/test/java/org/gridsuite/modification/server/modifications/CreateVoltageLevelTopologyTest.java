/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.dto.CreateVoltageLevelTopologyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.List;
import java.util.UUID;

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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("{\"voltageLevelId\":\"vl1\"}", modificationInfos.getMessageValues());
    }

}
