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
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularLoadModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                LoadModificationInfos.builder().equipmentId("v1load").constantReactivePower(new AttributeModification<>(300., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v2load").constantReactivePower(new AttributeModification<>(300., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v3load").constantReactivePower(new AttributeModification<>(300., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .equipmentType(IdentifiableType.LOAD)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                LoadModificationInfos.builder().equipmentId("v1load").constantReactivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v2load").constantReactivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v3load").constantReactivePower(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .equipmentType(IdentifiableType.LOAD)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(300., getNetwork().getLoad("v1load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v2load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v3load").getQ0(), 0.001);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(0., getNetwork().getLoad("v1load").getQ0(), 0.001);
        assertEquals(0., getNetwork().getLoad("v2load").getQ0(), 0.001);
        assertEquals(0., getNetwork().getLoad("v3load").getQ0(), 0.001);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(IdentifiableType.LOAD.name(), createdValues.get(TABULAR_EQUIPMENT_TYPE));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(IdentifiableType.LOAD.name(), updatedValues.get(TABULAR_EQUIPMENT_TYPE));
    }
}
