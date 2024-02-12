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
import org.testcontainers.shaded.org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.IntStream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.assertj.core.api.Assertions.assertThat;
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
        Pair<UUID, ModificationInfos> tabularWith1Modification = createTabularGeneratorModification(1);
        reset();
        ModificationInfos tabularWith1ModificationInfos = ApiUtils.getModification(mockMvc, tabularWith1Modification.getLeft()); // Getting one tabular modification with one sub-modification
        assertSelectCount(4); // 4 before improvements
        assertThat(tabularWith1Modification.getRight())
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(tabularWith1ModificationInfos);

        Pair<UUID, ModificationInfos> tabularWith3Modification = createTabularGeneratorModification(3);
        reset();
        ModificationInfos tabularWith3ModificationInfos = ApiUtils.getModification(mockMvc, tabularWith3Modification.getLeft()); // Getting one tabular modification with three sub-modifications
        assertSelectCount(4); // 6 before improvements
        assertThat(tabularWith3Modification.getRight())
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .ignoringCollectionOrder() // TODO: Should we care about sub-modifications order in tabular modifications ?
            .isEqualTo(tabularWith3ModificationInfos);
    }

    @Test
    public void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        Pair<UUID, ModificationInfos> tabularWith1Modification = createTabularGeneratorModification(1);
        Pair<UUID, ModificationInfos> tabularWith3Modification = createTabularGeneratorModification(3);

        reset();
        List<ModificationInfos> tabularModifications = ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(8); // 10 before improvements
        assertThat(List.of(tabularWith1Modification.getRight(), tabularWith3Modification.getRight()))
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .ignoringCollectionOrder() // TODO: Should we care about sub-modifications order in tabular modifications ?
            .isEqualTo(tabularModifications);
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

    private Pair<UUID, ModificationInfos> createTabularGeneratorModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.GENERATOR_MODIFICATION)
            .modifications(createGeneratorModificationList(qty))
            .build();
        UUID uuid = saveModification(tabularModification);
        tabularModification.setUuid(uuid);
        return Pair.of(uuid, tabularModification);
    }

    private List<ModificationInfos> createGeneratorModificationList(int qty) {
        return IntStream.range(0, qty)
            .mapToObj(i ->
                (ModificationInfos) GeneratorModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .maxActivePower(new AttributeModification<>(300., OperationType.SET))
                    .properties(List.of(
                        ModificationCreation.getFreeProperty(),
                        ModificationCreation.getFreeProperty("test", "value")))
                    .reactiveCapabilityCurvePoints(List.of(
                        ReactiveCapabilityCurveModificationInfos.builder().p(10.).oldP(15.).build(),
                        ReactiveCapabilityCurveModificationInfos.builder().qmaxP(12.).oldQmaxP(17.).build(),
                        ReactiveCapabilityCurveModificationInfos.builder().qminP(5.).qmaxP(5.).p(5.).build()))
                    .build())
            .toList();
    }
}
