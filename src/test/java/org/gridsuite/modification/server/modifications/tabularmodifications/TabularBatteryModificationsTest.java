/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.BatteryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularBatteryModificationsTest extends AbstractTabularModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                BatteryModificationInfos.builder().equipmentId("v1Battery").maxActivePower(new AttributeModification<>(50., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v2Battery").minActivePower(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v3Battery").activePowerSetpoint(new AttributeModification<>(5., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("unknownBattery").reactivePowerSetpoint(new AttributeModification<>(500., OperationType.SET)).build()
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
                BatteryModificationInfos.builder().equipmentId("v1Battery").minActivePower(new AttributeModification<>(3., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v2Battery").maxActivePower(new AttributeModification<>(30., OperationType.SET)).build(),
                BatteryModificationInfos.builder().equipmentId("v3Battery").activePowerSetpoint(new AttributeModification<>(6., OperationType.SET)).build()
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
        assertLogMessage("BATTERY_NOT_FOUND : Battery unknownBattery does not exist in network", ModificationType.BATTERY_MODIFICATION.name() + "1", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(15., getNetwork().getBattery("v1Battery").getMaxP(), 0.001);
        assertEquals(0., getNetwork().getBattery("v2Battery").getMinP(), 0.001);
        assertEquals(1., getNetwork().getBattery("v3Battery").getTargetP(), 0.001);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.BATTERY_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.BATTERY_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }

    @Override
    protected UUID createTabularModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.BATTERY_MODIFICATION)
            .modifications(createBatteryModificationList(qty))
            .build();
        return saveModification(tabularModification);
    }

    private List<ModificationInfos> createBatteryModificationList(int qty) {
        List<ModificationInfos> modifications = new ArrayList<>();
        for (int i = 0; i < qty; i++) {
            modifications.add(
                BatteryModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .maxActivePower(new AttributeModification<>(300., OperationType.SET))
                    .properties(List.of(ModificationCreation.getFreeProperty()))
                    .build()
            );
        }
        return modifications;
    }
}
