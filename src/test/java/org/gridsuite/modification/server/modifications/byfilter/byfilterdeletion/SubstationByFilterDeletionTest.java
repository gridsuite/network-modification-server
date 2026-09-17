/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@Tag("IntegrationTest")
class SubstationByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String SUBSTATION_ID_1 = "s1";
    private static final String SUBSTATION_ID_2 = "s2";
    private static final String SUBSTATION_ID_3 = "s3";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(SUBSTATION_ID_1, SUBSTATION_ID_2),
            FILTER_ID_2, Set.of(SUBSTATION_ID_3)
    );

    @Override
    protected Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1, filter2))
                .build();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_1));
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_2));
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_3));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getSubstation(SUBSTATION_ID_1));
        assertNotNull(getNetwork().getSubstation(SUBSTATION_ID_2));
        assertNotNull(getNetwork().getSubstation(SUBSTATION_ID_3));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.SUBSTATION;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.SUBSTATION;
    }
}
