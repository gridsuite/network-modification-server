/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.modifications.TabularModification;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 */
@Tag("IntegrationTest")
class TabularShuntCompensatorModificationsTest extends AbstractNetworkModificationTest {

    @Mock
    private Network network;

    @Mock
    private Reporter reporter;

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
                .modificationType("SHUNT_COMPENSATOR_MODIFICATION")
                .modifications(modifications)
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
                .modificationType("SHUNT_COMPENSATOR_MODIFICATION")
                .modifications(modifications)
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
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("SHUNT_COMPENSATOR_MODIFICATION", createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("SHUNT_COMPENSATOR_MODIFICATION", updatedValues.get("tabularModificationType"));
    }

    @Test
    void testCheckModificationConflict() {
        var shuntModification = ShuntCompensatorModificationInfos
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .maxSusceptance(AttributeModification.toAttributeModification(10.0, OperationType.SET))
                .build();

        var tabularModification = TabularModificationInfos
                .builder()
                .modificationType("SHUNT_COMPENSATOR_MODIFICATION")
                .modifications(Collections.singletonList(shuntModification))
                .build();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");

        boolean result = ((TabularModification) tabularModification.toModification())
                .checkShuntCompensatorModification(network, shuntModification, reporter);

        assertTrue(result);
        verify(reporter).report(argThat(report -> report.getValue(Report.REPORT_SEVERITY_KEY) == TypedValue.WARN_SEVERITY));
    }

    @Test
    void testCheckModificationNonLinear() {
        var shuntModification = ShuntCompensatorModificationInfos
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .build();

        var tabularModification = TabularModificationInfos
                .builder()
                .modificationType("SHUNT_COMPENSATOR_MODIFICATION")
                .modifications(Collections.singletonList(shuntModification))
                .build();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.NON_LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");

        boolean result = ((TabularModification) tabularModification.toModification())
                .checkShuntCompensatorModification(network, shuntModification, reporter);

        assertFalse(result);
        verify(reporter).report(argThat(report -> report.getValue(Report.REPORT_SEVERITY_KEY) == TypedValue.ERROR_SEVERITY));
    }

    @Test
    void testCheckModificationOK() {
        var shuntModification = ShuntCompensatorModificationInfos
                .builder()
                .equipmentId("id")
                .maxQAtNominalV(AttributeModification.toAttributeModification(1.0, OperationType.SET))
                .build();

        var tabularModification = TabularModificationInfos
                .builder()
                .modificationType("SHUNT_COMPENSATOR_MODIFICATION")
                .modifications(Collections.singletonList(shuntModification))
                .build();

        when(network.getShuntCompensator("id")).thenReturn(shuntCompensator);
        when(shuntCompensator.getModelType()).thenReturn(ShuntCompensatorModelType.LINEAR);
        when(shuntCompensator.getId()).thenReturn("id");

        boolean result = ((TabularModification) tabularModification.toModification())
                .checkShuntCompensatorModification(network, shuntModification, reporter);

        assertTrue(result);
        verify(reporter, never()).report(any());
    }
}
