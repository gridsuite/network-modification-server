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
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.testcontainers.shaded.org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
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
        assertTabularModificationsEquals(tabularWith1Modification.getRight(), tabularWith1ModificationInfos);

        Pair<UUID, ModificationInfos> tabularWith3Modification = createTabularGeneratorModification(3);
        reset();
        ModificationInfos tabularWith3ModificationInfos = ApiUtils.getModification(mockMvc, tabularWith3Modification.getLeft()); // Getting one tabular modification with three sub-modifications
        assertSelectCount(4); // 6 before improvements
        assertTabularModificationsEquals(tabularWith3Modification.getRight(), tabularWith3ModificationInfos);
    }

    @Test
    public void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        List<ModificationInfos> tabularModifications = ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(8); // 10 before improvements
        assertTabularModificationsEquals(modifications.stream().map(Pair::getRight).toList(), tabularModifications);
    }

    /*
    POST /v1/groups SQL requests analysis
    First we select the modifications to copy:
    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 3 select per generator tabular modification (get IDs, get reactive capability curve points, get properties)
    - 1 select on group to check if it exists before the save
    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)
    Then modifications order is set:
    - 1 update in modification for orders (batchSize: number of tabular modifications)
    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)
    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPostGroups() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId(), targetGroupUuid);
        TestUtils.assertRequestsCount(9, 8, 2, 0); // (13, 8, 2, 0) before improvements
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    public void testSqlRequestsCountOnPostGroups2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId(), targetGroupUuid);
        TestUtils.assertRequestsCount(15, 9, 2, 0); // (95, 9, 2, 0) before improvements, why one additional insert ? It feels batch_size is limited at 100 for insertions and is it reached for reactive_capability_curve_points
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    /*
    PUT /v1/groups/{groupUuid}/duplications SQL requests analysis
    First we select the modifications to copy:
    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 3 select per generator tabular modification (get IDs, get reactive capability curve points, get properties)
    - 1 select on group to check if it exists before the save
    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)
    Then modifications order is set:
    - 1 update in modification for orders (batchSize: number of tabular modifications)
    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)
    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPutGroupsDuplications() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), targetGroupUuid, getNetworkId());
        TestUtils.assertRequestsCount(9, 8, 2, 0); // (19, 8, 2, 0) before improvements
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    public void testSqlRequestsCountOnPutGroupsDuplications2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), targetGroupUuid, getNetworkId());
        TestUtils.assertRequestsCount(15, 9, 2, 0); // (107, 9, 2, 0) before improvements, why one additional insert ? It feels batch_size is limited at 100 for insertions and is it reached for reactive_capability_curve_points
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    /*
    PUT /v1/groups/{groupUuid}?action=COPY SQL requests analysis
    First we select the modifications to copy:
    - 1 select to find modifications of this group
    - 3 select per generator tabular modification (get IDs, get reactive capability curve points, get properties)
    - 1 select on group to check if it exists before the save
    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)
    Then modifications order is set:
    - 1 update in modification for orders (batchSize: number of tabular modifications)
    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)
    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPutGroupsWithCopy() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, targetGroupUuid, modifications.stream().map(Pair::getLeft).toList(), getNetworkId());
        TestUtils.assertRequestsCount(8, 8, 2, 0); // (14, 8, 2, 0) before improvements
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    public void testSqlRequestsCountOnPutGroupsWithCopy2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, targetGroupUuid, modifications.stream().map(Pair::getLeft).toList(), getNetworkId());
        TestUtils.assertRequestsCount(14, 9, 2, 0); // (26, 9, 2, 0) before improvements, why one additional insert ? It feels batch_size is limited at 100 for insertions and is it reached for reactive_capability_curve_points
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    /*
    POST /v1/network-modifications/duplicate SQL requests analysis
    First we select the modifications to copy:
    - 1 select to find modifications of this group
    - 3 select per generator tabular modification (get IDs, get reactive capability curve points, get properties)
    Then we insert the new modifications in the new group:
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)
    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)
    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPostNetworkModificationsDuplicate() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(7, 7, 1, 0); // (11, 7, 1, 0) before improvements
        assertTabularModificationsEquals(modifications, idsMapping);
    }

    @Test
    public void testSqlRequestsCountOnPostNetworkModificationsDuplicate2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();

        reset();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(13, 8, 1, 0); // (93, 8, 1, 0) before improvements, why one additional insert ? Maybe insertion batch size limit but not sure
        assertTabularModificationsEquals(modifications, idsMapping);
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

    private List<Pair<UUID, ModificationInfos>> createFewTabularModifications() {
        Pair<UUID, ModificationInfos> tabular1 = createTabularGeneratorModification(1);
        Pair<UUID, ModificationInfos> tabular2 = createTabularGeneratorModification(3);

        return List.of(tabular1, tabular2);
    }

    private List<Pair<UUID, ModificationInfos>> createMoreTabularModifications() {
        Pair<UUID, ModificationInfos> tabular1 = createTabularGeneratorModification(1);
        Pair<UUID, ModificationInfos> tabular2 = createTabularGeneratorModification(3);
        Pair<UUID, ModificationInfos> tabular3 = createTabularGeneratorModification(10);
        Pair<UUID, ModificationInfos> tabular4 = createTabularGeneratorModification(30);

        return List.of(tabular1, tabular2, tabular3, tabular4);
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

    private void assertTabularModificationsEquals(List<Pair<UUID, ModificationInfos>> expectedModifications, UUID groupUuid) throws Exception {
        List<ModificationInfos> tabularModifications = ApiUtils.getGroupModifications(mockMvc, groupUuid);
        assertTabularModificationsEquals(expectedModifications.stream().map(Pair::getRight).toList(), tabularModifications);
    }

    private void assertTabularModificationsEquals(List<Pair<UUID, ModificationInfos>> expectedModifications, Map<UUID, UUID> idsMapping) {
        Map<UUID, ModificationInfos> retrievedModifications = idsMapping.values()
            .stream()
            .map(id -> {
                try {
                    return ApiUtils.getModification(mockMvc, id);
                } catch (Exception e) {
                    return null; // Not important, comparison will fail if some modification fetch fails
                }
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toMap(
                ModificationInfos::getUuid,
                Function.identity()
            ));
        List<ModificationInfos> sourceModifications = expectedModifications.stream().map(Pair::getRight).toList();
        // Ordering the retrieved list based on the source <-> target mapping
        List<ModificationInfos> targetModifications = sourceModifications.stream().map(m -> retrievedModifications.get(idsMapping.get(m.getUuid()))).toList();

        assertTabularModificationsEquals(sourceModifications, targetModifications);
    }

    private void assertTabularModificationsEquals(List<ModificationInfos> expectedModifications, List<ModificationInfos> modificationInfos) {
        assertThat(expectedModifications)
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(modificationInfos);
    }

    private void assertTabularModificationsEquals(ModificationInfos expectedModificationInfos, ModificationInfos modificationInfos) {
        assertThat(expectedModificationInfos)
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(modificationInfos);
    }
}
