/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.dto.TabularModificationInfos.TABULAR_EQUIPMENT_TYPE;
import static org.junit.Assert.assertEquals;

/**
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularVoltageLevelModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                VoltageLevelModificationInfos.builder().equipmentId("v1").nominalVoltage(new AttributeModification<>(300., OperationType.SET)).highVoltageLimit(new AttributeModification<>(400., OperationType.SET)).lowVoltageLimit(new AttributeModification<>(299., OperationType.SET)).build(),
                VoltageLevelModificationInfos.builder().equipmentId("v2").nominalVoltage(new AttributeModification<>(300., OperationType.SET)).highVoltageLimit(new AttributeModification<>(400., OperationType.SET)).lowVoltageLimit(new AttributeModification<>(299., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                VoltageLevelModificationInfos.builder().equipmentId("v1").nominalVoltage(new AttributeModification<>(500., OperationType.SET)).highVoltageLimit(new AttributeModification<>(502., OperationType.SET)).lowVoltageLimit(new AttributeModification<>(499., OperationType.SET)).build(),
                VoltageLevelModificationInfos.builder().equipmentId("v2").nominalVoltage(new AttributeModification<>(500., OperationType.SET)).highVoltageLimit(new AttributeModification<>(502., OperationType.SET)).lowVoltageLimit(new AttributeModification<>(499., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(300., getNetwork().getVoltageLevel("v1").getNominalV(), 0.001);
        assertEquals(299., getNetwork().getVoltageLevel("v1").getLowVoltageLimit(), 0.001);
        assertEquals(400., getNetwork().getVoltageLevel("v1").getHighVoltageLimit(), 0.001);
        assertEquals(300., getNetwork().getVoltageLevel("v2").getNominalV(), 0.001);
        assertEquals(299., getNetwork().getVoltageLevel("v2").getLowVoltageLimit(), 0.001);
        assertEquals(400., getNetwork().getVoltageLevel("v2").getHighVoltageLimit(), 0.001);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(380.0, getNetwork().getVoltageLevel("v1").getNominalV(), 0.001);
        assertEquals(Double.NaN, getNetwork().getVoltageLevel("v1").getLowVoltageLimit(), 0.001);
        assertEquals(Double.NaN, getNetwork().getVoltageLevel("v1").getHighVoltageLimit(), 0.001);
        assertEquals(225.0, getNetwork().getVoltageLevel("v2").getNominalV(), 0.001);
        assertEquals(Double.NaN, getNetwork().getVoltageLevel("v2").getLowVoltageLimit(), 0.001);
        assertEquals(Double.NaN, getNetwork().getVoltageLevel("v2").getHighVoltageLimit(), 0.001);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(IdentifiableType.VOLTAGE_LEVEL.name(), createdValues.get(TABULAR_EQUIPMENT_TYPE));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(IdentifiableType.VOLTAGE_LEVEL.name(), updatedValues.get(TABULAR_EQUIPMENT_TYPE));
    }
}
