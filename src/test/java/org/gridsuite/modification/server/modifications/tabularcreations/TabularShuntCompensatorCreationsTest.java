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
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.dto.TabularCreationInfos;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
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
class TabularShuntCompensatorCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
                ShuntCompensatorCreationInfos.builder()
                        .stashed(false)
                        .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                        .equipmentId("shuntOneId")
                        .equipmentName("hop")
                        .maximumSectionCount(10)
                        .sectionCount(6)
                        .maxSusceptance(0.)
                        .voltageLevelId("v2")
                        .busOrBusbarSectionId("1B")
                        .connectionName("cn")
                        .connectionPosition(99)
                        .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                        .properties(List.of(FreePropertyInfos.builder().name("PROPERTY_NAME").value("PROPERTY_VALUE").build()))
                        .build()
        );
        return TabularCreationInfos.builder()
            .creationType(ModificationType.SHUNT_COMPENSATOR_CREATION)
            .creations(creations)
            .stashed(false)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> creations = List.of(
                ShuntCompensatorCreationInfos.builder()
                        .stashed(false)
                        .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                        .equipmentId("shuntOneId")
                        .equipmentName("New name")
                        .maximumSectionCount(10)
                        .sectionCount(6)
                        .maxSusceptance(0.)
                        .build()
        );
        return TabularCreationInfos.builder()
                .creationType(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .creations(creations)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getShuntCompensator("shuntOneId"));
        assertLogMessage("Tabular creation: 1 shunt compensator have been created", "network.modification.tabular.creation", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getShuntCompensator("shuntOneId"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_CREATION.name(), updatedValues.get("tabularCreationType"));
    }
}
