/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@Tag("IntegrationTest")
class EquipmentByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String LOAD_ID_1 = "load1";
    private static final String LOAD_ID_2 = "load2";
    private static final String LOAD_ID_3 = "load3";
    private static final String LOAD_ID_4 = "load4";
    private static final String LOAD_ID_7 = "load7";
    private static final String LOAD_ID_11 = "load11";
    private static final String LOAD_ID_12 = "load12";
    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(LOAD_ID_1, LOAD_ID_2),
            FILTER_ID_2, Set.of(LOAD_ID_3, LOAD_ID_4),
            FILTER_ID_3, Set.of(LOAD_ID_7, LOAD_ID_11, LOAD_ID_12)
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

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter3")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1, filter2, filter3))
                .build();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createLoadNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getLoad(LOAD_ID_1));
        assertNull(getNetwork().getLoad(LOAD_ID_2));
        assertNull(getNetwork().getLoad(LOAD_ID_3));
        assertNull(getNetwork().getLoad(LOAD_ID_4));
        assertNull(getNetwork().getLoad(LOAD_ID_7));
        assertNull(getNetwork().getLoad(LOAD_ID_11));
        assertNull(getNetwork().getLoad(LOAD_ID_12));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLoad(LOAD_ID_1));
        assertNotNull(getNetwork().getLoad(LOAD_ID_2));
        assertNotNull(getNetwork().getLoad(LOAD_ID_3));
        assertNotNull(getNetwork().getLoad(LOAD_ID_4));
        assertNotNull(getNetwork().getLoad(LOAD_ID_7));
        assertNotNull(getNetwork().getLoad(LOAD_ID_11));
        assertNotNull(getNetwork().getLoad(LOAD_ID_12));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LOAD;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LOAD;
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.SUBSTATION));
    }
}
