/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.gridsuite.modification.server.modifications.TabularModification;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author SARTORI David <david.sartori_externe@rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularShuntCompensatorModificationsTest extends AbstractTabularModificationTest {

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
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
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
                .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
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
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.SHUNT_COMPENSATOR_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }

    @Test
    public void testCheckModificationConflict() {
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

        tabularModification.checkShuntCompensatorModification(network, shuntModification, reporter);

        shuntModification.setShuntCompensatorType(AttributeModification.toAttributeModification(ShuntCompensatorType.CAPACITOR, OperationType.SET));
        tabularModification.checkShuntCompensatorModification(network, shuntModification, reporter);

        shuntModification.setMaxQAtNominalV(null);
        tabularModification.checkShuntCompensatorModification(network, shuntModification, reporter);

        verify(reporter, times(3)).report(argThat(report -> report.getValue(Report.REPORT_SEVERITY_KEY) == TypedValue.WARN_SEVERITY));
    }

    @Test
    public void testCheckModificationNonLinear() {
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

        tabularModification.checkShuntCompensatorModification(network, shuntModification, reporter);
        verify(reporter).report(argThat(report -> report.getValue(Report.REPORT_SEVERITY_KEY) == TypedValue.ERROR_SEVERITY));
    }

    @Test
    public void testCheckModificationOK() {
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

        tabularModification.checkShuntCompensatorModification(network, shuntModification, reporter);
        verify(reporter, never()).report(any());
    }

    @Override
    protected UUID createTabularModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.SHUNT_COMPENSATOR_MODIFICATION)
            .modifications(createShuntCompensatorModificationList(qty))
            .build();
        return saveModification(tabularModification);
    }

    private List<ModificationInfos> createShuntCompensatorModificationList(int qty) {
        List<ModificationInfos> modifications = new ArrayList<>();
        for (int i = 0; i < qty; i++) {
            modifications.add(
                ShuntCompensatorModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .properties(List.of(ModificationCreation.getFreeProperty()))
                    .build()
            );
        }
        return modifications;
    }
}
