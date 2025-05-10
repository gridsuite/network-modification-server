/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
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
                BatteryModificationInfos.builder().equipmentId("v3Battery").targetP(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("unknownBattery").targetQ(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.BATTERY_MODIFICATION)
                .modifications(modifications)
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
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(50., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(5., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
        assertLogMessage("BATTERY_NOT_FOUND : Battery unknownBattery does not exist in network", "network.modification.tabular.modification.exception", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(15., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(0., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(1., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }
}
