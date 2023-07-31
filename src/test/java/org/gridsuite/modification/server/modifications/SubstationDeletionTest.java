/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
public class SubstationDeletionTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .equipmentType("SUBSTATION")
                .equipmentId("s1")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .equipmentType("LINE")
                .equipmentId("v2")
                .build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getSubstation("s1"));
        assertNull(getNetwork().getVoltageLevel("v1"));
        assertNull(getNetwork().getVoltageLevel("v2"));
        assertNull(getNetwork().getLoad("v1load"));
        assertNull(getNetwork().getLccConverterStation("v1lcc"));
        assertNull(getNetwork().getSwitch("v1d1"));
        assertNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getHvdcLine("hvdcLine"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getSubstation("s1"));
    }
}
