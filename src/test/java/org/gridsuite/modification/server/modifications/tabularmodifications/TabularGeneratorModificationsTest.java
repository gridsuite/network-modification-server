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
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.ApiUtils;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularGeneratorModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("unknownGenerator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(500., getNetwork().getGenerator("idGenerator").getMaxP(), 0.001);
        assertEquals(500., getNetwork().getGenerator("v5generator").getMaxP(), 0.001);
        assertEquals(500., getNetwork().getGenerator("v6generator").getMaxP(), 0.001);
        assertLogMessage("GENERATOR_NOT_FOUND : Generator unknownGenerator does not exist in network", ModificationType.GENERATOR_MODIFICATION.name() + "1", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(1000., getNetwork().getGenerator("idGenerator").getMaxP(), 0.001);
        assertEquals(1000., getNetwork().getGenerator("v5generator").getMaxP(), 0.001);
        assertEquals(1000., getNetwork().getGenerator("v6generator").getMaxP(), 0.001);
    }

    @Test
    public void testSqlRequestsCountOnGetModification() throws Exception {
        UUID tabularWith1ModificationUuid = createTabularGeneratorModification(1);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith1ModificationUuid); // Getting one tabular modification with one sub-modification
        assertSelectCount(3);

        UUID tabularWith3ModificationUuid = createTabularGeneratorModification(3);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith3ModificationUuid); // Getting one tabular modification with three sub-modifications
        assertSelectCount(3);
    }

    @Test
    public void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        createTabularGeneratorModification(1);
        createTabularGeneratorModification(3);

        reset();
        ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(6);
    }

    private UUID createTabularGeneratorModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.GENERATOR_MODIFICATION)
            .modifications(createGeneratorModificationList(qty))
            .build();
        return saveModification(tabularModification);
    }

    private List<ModificationInfos> createGeneratorModificationList(int qty) {
        List<ModificationInfos> modifications = new ArrayList<>();
        for (int i = 0; i <= qty; i++) {
            modifications.add(
                GeneratorModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .maxActivePower(new AttributeModification<>(300., OperationType.SET))
                    .properties(List.of(ModificationCreation.getFreeProperty()))
                    .build()
            );
        }
        return modifications;
    }

    @Test
    public void testAllModificationsHaveFailed() throws Exception {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxActivePower(new AttributeModification<>(-300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxActivePower(new AttributeModification<>(-300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxActivePower(new AttributeModification<>(-300., OperationType.SET)).build()
        );
        ModificationInfos modificationInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .build();
        String modificationToCreateJson = mapper.writeValueAsString(modificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();
        assertLogMessage("Tabular modification: No generators have been modified", "tabularGENERATOR_MODIFICATIONError", reportService);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.GENERATOR_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.GENERATOR_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }
}
