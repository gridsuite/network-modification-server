/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.BatteryCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.dto.TabularCreationInfos;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularBatteryCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
            BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId("B1").equipmentName("b1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .minP(100.0).maxP(600.0).targetP(400.).targetQ(50.)
                .minQ(20.0).maxQ(25.0).droop(5f).participate(true).reactiveCapabilityCurve(true)
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(2.0, 3.0, 3.1),
                    new ReactiveCapabilityCurvePointsInfos(5.6, 9.8, 10.8)))
                .properties(List.of(
                    ModificationCreation.getFreeProperty("test", "value"),
                    ModificationCreation.getFreeProperty("test2", "value2")))
                .build()
        );
        return TabularCreationInfos.builder()
            .creationType(ModificationType.BATTERY_CREATION)
            .creations(creations)
            .stashed(false)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> creations = List.of(
                BatteryCreationInfos.builder()
                        .stashed(false)
                        .equipmentId("B1").equipmentName("newName")
                        .minP(101.0).maxP(601.0).targetP(401.).targetQ(51.)
                        .build()
        );
        return TabularCreationInfos.builder()
                .creationType(ModificationType.BATTERY_CREATION)
                .creations(creations)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getBattery("B1"));
        assertLogMessage("Tabular creation: 1 battery have been created", "network.modification.tabular.creation", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getBattery("B1"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.BATTERY_CREATION.name(), updatedValues.get("tabularCreationType"));
    }
}
