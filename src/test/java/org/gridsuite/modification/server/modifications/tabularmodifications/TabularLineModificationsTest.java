/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Anis Touri <anis.touri at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularLineModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                LineModificationInfos.builder().equipmentId("line1").r(new AttributeModification<>(10., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line2").x(new AttributeModification<>(20., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").g1(new AttributeModification<>(30., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").b1(new AttributeModification<>(40., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("unknownLine").b2(new AttributeModification<>(60., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                LineModificationInfos.builder().equipmentId("line1").r(new AttributeModification<>(1., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line2").r(new AttributeModification<>(2., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").g1(new AttributeModification<>(3., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").b1(new AttributeModification<>(4., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("unknownLine").b2(new AttributeModification<>(50., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(10., getNetwork().getLine("line1").getR(), 0.001);
        assertEquals(20., getNetwork().getLine("line2").getX(), 0.001);
        assertEquals(30., getNetwork().getLine("line3").getG1(), 0.001);
        assertEquals(40., getNetwork().getLine("line3").getB1(), 0.001);
        assertLogMessage("LINE_NOT_FOUND : Line unknownLine does not exist in network", ModificationType.LINE_MODIFICATION.name() + "1", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(1., getNetwork().getLine("line1").getR(), 0.001);
        assertEquals(5., getNetwork().getLine("line2").getX(), 0.001);
        assertEquals(5.5, getNetwork().getLine("line3").getG1(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LINE_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LINE_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }
}
