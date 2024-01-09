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
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
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
    public void testCheckSqlRequestsCount() throws Exception {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build()
        );
        ModificationInfos modificationInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .build();
        UUID modificationUuid = saveModification(modificationInfos);
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        assertSelectCount(3);

        modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxActivePower(new AttributeModification<>(300., OperationType.SET)).build()
        );
        modificationInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .build();
        modificationUuid = saveModification(modificationInfos);
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        // We check that the request count is not dependent on the number of sub modifications of the tabular modification (the JPA N+1 problem is correctly solved)
        assertSelectCount(3);
        reset();

        // We get the modifications of the group (so the 2 tabular modifications)
        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
                .andExpect(status().isOk());
        // We check that the request count is not dependent on the number of sub modifications of the tabular modification (the JPA N+1 problem is correctly solved)
        assertSelectCount(6);
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
        assertLogMessage("Tabular modification: No generators have been modified", "tabularModificationError", reportService);
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
