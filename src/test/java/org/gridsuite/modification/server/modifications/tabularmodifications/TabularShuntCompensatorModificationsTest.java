/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.modifications.TabularModification;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;

/**
 * @author SARTORI David <david.sartori_externe@rte-france.com>
 */
@Tag("IntegrationTest")
    class TabularShuntCompensatorModificationsTest extends AbstractNetworkModificationTest {

    @Mock
    private Network network;

    @Mock
    private ShuntCompensator shuntCompensator;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                ShuntCompensatorModificationInfos.builder().equipmentId("v2shunt").maximumSectionCount(new AttributeModification<>(100, OperationType.SET)).sectionCount(new AttributeModification<>(10, OperationType.SET)).build(),
                ShuntCompensatorModificationInfos.builder().equipmentId("v5shunt").maximumSectionCount(new AttributeModification<>(200, OperationType.SET)).sectionCount(new AttributeModification<>(20, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(true).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                ShuntCompensatorModificationInfos.builder().equipmentId("v2shunt").maximumSectionCount(new AttributeModification<>(500, OperationType.SET)).sectionCount(new AttributeModification<>(50, OperationType.SET)).build(),
                ShuntCompensatorModificationInfos.builder().equipmentId("v5shunt").maximumSectionCount(new AttributeModification<>(500, OperationType.SET)).sectionCount(new AttributeModification<>(50, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(100, getNetwork().getShuntCompensator("v2shunt").getMaximumSectionCount());
        assertEquals(10, getNetwork().getShuntCompensator("v2shunt").getSectionCount());
        assertEquals(200, getNetwork().getShuntCompensator("v5shunt").getMaximumSectionCount());
        assertEquals(20, getNetwork().getShuntCompensator("v5shunt").getSectionCount());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(3, getNetwork().getShuntCompensator("v2shunt").getMaximumSectionCount());
        assertEquals(2, getNetwork().getShuntCompensator("v2shunt").getSectionCount());
        assertEquals(3, getNetwork().getShuntCompensator("v5shunt").getMaximumSectionCount());
        assertEquals(2, getNetwork().getShuntCompensator("v5shunt").getSectionCount());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }

    @Test
    void testCheckModificationConflict() {
        var shuntModification = ShuntCompensatorModificationInfos
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .maxSusceptance(AttributeModification.toAttributeModification(10.0, OperationType.SET))
                .build();

        var tabularModificationInfos = TabularModificationInfos
                .builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(Collections.singletonList(shuntModification))
                .build();

        var tabularModification = (TabularModification) tabularModificationInfos.toModification();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles("i18n.reports")
                .withMessageTemplate("test")
                .build();

        tabularModification.checkShuntCompensatorModification(network, shuntModification, reportNode);

        shuntModification.setShuntCompensatorType(AttributeModification.toAttributeModification(ShuntCompensatorType.CAPACITOR, OperationType.SET));
        tabularModification.checkShuntCompensatorModification(network, shuntModification, reportNode);

        shuntModification.setMaxQAtNominalV(null);
        tabularModification.checkShuntCompensatorModification(network, shuntModification, reportNode);

        assertEquals(TypedValue.WARN_SEVERITY, reportNode.getChildren().get(0).getValues().get(ReportConstants.SEVERITY_KEY));
    }

    @Test
    void testCheckModificationNonLinear() {
        var shuntModification = ShuntCompensatorModificationInfos
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .build();

        var tabularModificationInfos = TabularModificationInfos
                .builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(Collections.singletonList(shuntModification))
                .build();

        var tabularModification = (TabularModification) tabularModificationInfos.toModification();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.NON_LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");

        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles("i18n.reports")
                .withMessageTemplate("test")
                .build();
        tabularModification.checkShuntCompensatorModification(network, shuntModification, reportNode);

        assertEquals(TypedValue.ERROR_SEVERITY, reportNode.getChildren().get(0).getValues().get(ReportConstants.SEVERITY_KEY));

    }

    @Test
    void testCheckModificationOK() {
        var shuntModification = ShuntCompensatorModificationInfos
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .build();

        var tabularModificationInfos = TabularModificationInfos
                .builder()
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
                .modifications(Collections.singletonList(shuntModification))
                .build();

        var tabularModification = (TabularModification) tabularModificationInfos.toModification();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");
        ReportNode reportNode = ReportNode.newRootReportNode()
                .withResourceBundles("i18n.reports")
                .withMessageTemplate("test")
                .build();

        tabularModification.checkShuntCompensatorModification(network, shuntModification, reportNode);
        assertEquals(0, reportNode.getChildren().size());
    }
}
