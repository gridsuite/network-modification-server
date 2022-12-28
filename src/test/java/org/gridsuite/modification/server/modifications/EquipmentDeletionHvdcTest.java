/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.powsybl.iidm.network.VariantManagerConstants;

public class EquipmentDeletionHvdcTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("HVDC_LINE")
                .equipmentId("hvdcLine")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("HVDC_LINE")
                .equipmentId("hvdcLineTest")
                .build();
    }

    @Override
    protected MatcherEquipmentDeletionInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos((EquipmentDeletionInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getHvdcLine("hvdcLine"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("hvdcLine", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getHvdcLine("hvdcLine"));
    }
}
