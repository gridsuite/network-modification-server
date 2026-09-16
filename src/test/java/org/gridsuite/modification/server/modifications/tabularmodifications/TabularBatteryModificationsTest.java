/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularPropertyInfos;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularBatteryModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                BatteryModificationInfos.builder().equipmentId("v1Battery").maxP(new AttributeModification<>(50., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v2Battery").minP(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v3Battery").targetP(new AttributeModification<>(5., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.BATTERY_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(true).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                BatteryModificationInfos.builder().equipmentId("v1Battery").minP(new AttributeModification<>(3., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v2Battery").maxP(new AttributeModification<>(30., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v3Battery").targetP(new AttributeModification<>(6., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.BATTERY_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .stashed(false)
                .build();
    }

    protected void assertAfterNetworkModificationCreation() {
        assertEquals(50., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
    }

    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(15., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(0., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(1., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        super.testCreate();
        assertAfterNetworkModificationCreation();
    }

    @Test
    @Override
    public void testCreateDisabledModification() throws Exception {
        super.testCreateDisabledModification();
        assertAfterNetworkModificationDeletion();
    }

    @Test
    @Override
    public void testDelete() throws Exception {
        super.testDelete();
        assertAfterNetworkModificationDeletion();
    }
}
