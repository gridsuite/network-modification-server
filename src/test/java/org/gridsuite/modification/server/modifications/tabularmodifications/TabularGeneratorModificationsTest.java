/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ApiUtils;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
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
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularGeneratorModificationsTest extends AbstractNetworkModificationTest {
    @Autowired
    private ModificationRepository modificationRepository;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("unknownGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build()
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
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxP(new AttributeModification<>(300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxP(new AttributeModification<>(300., OperationType.SET)).build()
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
        assertLogMessage("GENERATOR_NOT_FOUND : Generator unknownGenerator does not exist in network", "network.modification.tabular.modification.exception", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(1000., getNetwork().getGenerator("idGenerator").getMaxP(), 0.001);
        assertEquals(1000., getNetwork().getGenerator("v5generator").getMaxP(), 0.001);
        assertEquals(1000., getNetwork().getGenerator("v6generator").getMaxP(), 0.001);
    }

    @Test
    void testSqlRequestsCountOnGetModification() throws Exception {
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
    void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        List<ModificationInfos> tabularModifications = ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(8); // 10 before improvements
        assertTabularModificationsEquals(modifications.stream().map(Pair::getRight).toList(), tabularModifications);
    }

    /*
    POST /v1/groups SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    First we select the modifications to copy:
    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 6 selects: 3 per tabular modification (get IDs, get reactive capability curve points, get properties)
    - 1 select on group to check if it exists before the save
    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 2 inserts in modification for tabular modifications
    - 2 inserts in tabular_modification
    - batched* and reduced** 2000 inserts in modification for sub-modifications
    - batched* and reduced** 2000 inserts in sub-modification table
    - batched* and reduced** 2000 inserts in free_property
    - batched* and reduced** 2000 inserts in reactive_capability_curve_points
    Then modifications order is set:
    - 2 updates in modification for orders
    Then relation between tabular modifications and sub-modifications are set:
    - batched* and reduced** 2000 inserts in tabular_modification_modifications for the relation
    (optional) Then order of sub-modifications relations are set:
    - batched* but not reduced** 2000 updates in free_property

    *Batched means it requires less network connections to exchange all the requests, they are grouped by batches.
    **Reduced means several 'unitary' requests are merged into one request with several entries. It is a postrgreeSQL
    optimization to reduce the treated number of requests.
    NB: as a limitation of pgjdbc we have a maximum of 128 entries for each merged requests, as multiple entries increase
    response time, it is a better optimization to start multiplying requests instead of entries in one request.
     */
    @Test
    void testSqlRequestsCountOnPostGroups() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId(), targetGroupUuid);
        TestUtils.assertRequestsCount(10, 8, 1, 0); // (13, 8, 2, 0) before improvements
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPostGroups2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId(), targetGroupUuid);
        TestUtils.assertRequestsCount(16, 9, 1, 0); // (95, 9, 2, 0) before improvements, why one additional insert ? It feels batch_size is limited at 100 for insertions and is it reached for reactive_capability_curve_points
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    /*
    PUT /v1/groups/{groupUuid}?action=COPY SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    First we select the modifications to copy:
    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 6 selects: 3 per tabular modification (get IDs, get reactive capability curve points, get properties)
    - 1 select on group to check if it exists before the save
    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 2 inserts in modification for tabular modifications
    - 2 inserts in tabular_modification
    - batched* and reduced** 2000 inserts in modification for sub-modifications
    - batched* and reduced** 2000 inserts in sub-modification table
    - batched* and reduced** 2000 inserts in free_property
    - batched* and reduced** 2000 inserts in reactive_capability_curve_points
    Then modifications order is set:
    - 2 updates in modification for orders
    Then relation between tabular modifications and sub-modifications are set:
    - batched* and reduced** 2000 inserts in tabular_modification_modifications for the relation
    (optional) Then order of sub-modifications relations are set:
    - batched* but not reduced** 2000 updates in free_property

    *Batched means it requires less network connections to exchange all the requests, they are grouped by batches.
    **Reduced means several 'unitary' requests are merged into one request with several entries. It is a postrgreeSQL
    optimization to reduce the treated number of requests.
    NB: as a limitation of pgjdbc we have a maximum of 128 entries for each merged requests, as multiple entries increase
    response time, it is a better optimization to start multiplying requests instead of entries in one request.
     */
    @Test
    void testSqlRequestsCountOnPutGroupsDuplications() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), targetGroupUuid, getNetworkId());
        TestUtils.assertRequestsCount(10, 8, 1, 0); // (19, 8, 2, 0) before improvements
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPutGroupsDuplications2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), targetGroupUuid, getNetworkId());
        TestUtils.assertRequestsCount(16, 9, 1, 0); // (107, 9, 2, 0) before improvements, why one additional insert ? It feels batch_size is limited at 100 for insertions and is it reached for reactive_capability_curve_points
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    /*
    PUT /v1/groups/{groupUuid}?action=COPY SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    First we select the modifications to copy:
    - 1 select to find modifications of this group
    - 6 selects: 3 per tabular modification (get IDs, get reactive capability curve points, get properties)
    - 1 select on group to check if it exists before the save
    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 2 inserts in modification for tabular modifications
    - 2 inserts in tabular_modification
    - batched* and reduced** 2000 inserts in modification for sub-modifications
    - batched* and reduced** 2000 inserts in sub-modification table
    - batched* and reduced** 2000 inserts in free_property
    - batched* and reduced** 2000 inserts in reactive_capability_curve_points
    Then modifications order is set:
    - 2 updates in modification for orders
    Then relation between tabular modifications and sub-modifications are set:
    - batched* and reduced** 2000 inserts in tabular_modification_modifications for the relation
    (optional) Then order of sub-modifications relations are set:
    - batched* but not reduced** 2000 updates in free_property

    *Batched means it requires less network connections to exchange all the requests, they are grouped by batches.
    **Reduced means several 'unitary' requests are merged into one request with several entries. It is a postrgreeSQL
    optimization to reduce the treated number of requests.
    NB: as a limitation of pgjdbc we have a maximum of 128 entries for each merged requests, as multiple entries increase
    response time, it is a better optimization to start multiplying requests instead of entries in one request.
     */
    @Test
    void testSqlRequestsCountOnPutGroupsWithCopy() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, targetGroupUuid, modifications.stream().map(Pair::getLeft).toList(), getNetworkId());
        TestUtils.assertRequestsCount(9, 8, 1, 0); // (14, 8, 2, 0) before improvements
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPutGroupsWithCopy2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, targetGroupUuid, modifications.stream().map(Pair::getLeft).toList(), getNetworkId());
        TestUtils.assertRequestsCount(15, 9, 1, 0); // (26, 9, 2, 0) before improvements, why one additional insert ? It feels batch_size is limited at 100 for insertions and is it reached for reactive_capability_curve_points
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    /*
    POST /v1/network-modifications/duplicate SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    First we select the modifications to copy:
    - 1 select to find modifications of this group
    - 6 selects: 3 per tabular modification (get IDs, get reactive capability curve points, get properties)
    Then we insert the new modifications in the new group:
    - 2 inserts in modification for tabular modifications
    - 2 inserts in tabular_modification
    - batched* and reduced** 2000 inserts in modification for sub-modifications
    - batched* and reduced** 2000 inserts in sub-modification table
    - batched* and reduced** 2000 inserts in free_property
    - batched* and reduced** 2000 inserts in reactive_capability_curve_points
    Then modifications order is set:
    - 2 updates in modification for orders
    Then relation between tabular modifications and sub-modifications are set:
    - batched* and reduced** 2000 inserts in tabular_modification_modifications for the relation
    (optional) Then order of sub-modifications relations are set:
    - batched* but not reduced** 2000 updates in free_property

    *Batched means it requires less network connections to exchange all the requests, they are grouped by batches.
    **Reduced means several 'unitary' requests are merged into one request with several entries. It is a postrgreeSQL
    optimization to reduce the treated number of requests.
    NB: as a limitation of pgjdbc we have a maximum of 128 entries for each merged requests, as multiple entries increase
    response time, it is a better optimization to start multiplying requests instead of entries in one request.
     */
    @Test
    void testSqlRequestsCountOnPostNetworkModificationsDuplicate() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(7, 7, 1, 0); // (11, 7, 1, 0) before improvements
        assertTabularModificationsEquals(modifications, idsMapping);
    }

    @Test
    void testSqlRequestsCountOnPostNetworkModificationsDuplicate2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();

        reset();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(13, 8, 1, 0); // (93, 8, 1, 0) before improvements, why one additional insert ? Maybe insertion batch size limit but not sure
        assertTabularModificationsEquals(modifications, idsMapping);
    }

    /*
    DELETE /v1/groups/{groupUuid} SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 2 selects to retrieve tabular sub-modifications IDs (1 per tabular modification)
    - 14 deletes (7 per tabular modification):
        - delete reactive_capability_curve_points
        - delete free_property
        - delete generator_modification
        - delete tabular_modifications_modification
        - delete modification for generators
        - delete tabular_modification
        - delete modification for tabular modifications
    - 1 select on group generated by the JPA delete of the group
    - 1 delete of the group
     */
    @Test
    void testSqlRequestsCountOnDeleteGroup() throws Exception {
        createFewTabularModifications();

        reset();
        ApiUtils.deleteGroup(mockMvc, getGroupId());
        // It is actually (8, 0, 0, 15) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(8, 0, 0, 1);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteGroup2() throws Exception {
        createMoreTabularModifications();

        reset();
        ApiUtils.deleteGroup(mockMvc, getGroupId());
        // It is actually (12, 0, 0, 29) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(12, 0, 0, 1);
        assertEquals(0, modificationRepository.count());
    }

    /*
    DELETE /v1/groups/{groupUuid}/stashed-modifications SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 2 selects to retrieve tabular sub-modifications IDs (1 per tabular modification)
    - 14 deletes (7 per tabular modification):
        - delete reactive_capability_curve_points
        - delete free_property
        - delete generator_modification
        - delete tabular_modifications_modification
        - delete modification for generators
        - delete tabular_modification
        - delete modification for tabular modifications
     */
    @Test
    void testSqlRequestsCountOnDeleteStashedInGroup() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        ApiUtils.stashNetworkModifications(mockMvc, modifications.stream().map(Pair::getLeft).toList());

        reset();
        ApiUtils.deleteStashedInGroup(mockMvc, getGroupId());
        // It is actually (6, 0, 0, 14) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(6, 0, 0, 0);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteStashedInGroup2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        ApiUtils.stashNetworkModifications(mockMvc, modifications.stream().map(Pair::getLeft).toList());

        reset();
        ApiUtils.deleteStashedInGroup(mockMvc, getGroupId());
        // It is actually (10, 0, 0, 21) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(10, 0, 0, 0);
        assertEquals(0, modificationRepository.count());
    }

    /*
    DELETE /v1/network-modifications SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 2 selects to retrieve tabular sub-modifications IDs (1 per tabular modification)
    - 14 deletes (7 per tabular modification):
        - delete reactive_capability_curve_points
        - delete free_property
        - delete generator_modification
        - delete tabular_modifications_modification
        - delete modification for generators
        - delete tabular_modification
        - delete modification for tabular modifications
     */
    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsInGroup() throws Exception {
        createFewTabularModifications();

        reset();
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId());
        // It is actually (6, 0, 0, 14) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(6, 0, 0, 0);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsInGroup2() throws Exception {
        createMoreTabularModifications();

        reset();
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId());
        // It is actually (10, 0, 0, 21) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(10, 0, 0, 0);
        assertEquals(0, modificationRepository.count());
    }

    /*
    DELETE /v1/network-modifications SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    - 1 select on group to check if it exists
    - 1 select to find modifications of this group
    - 1 update to maintain modifications order
    - 2 select to retrieve tabular sub-modifications IDs (1 per tabular modification)
    - 14 deletes (7 per tabular modification):
        - delete reactive_capability_curve_points
        - delete free_property
        - delete generator_modification
        - delete tabular_modifications_modification
        - delete modification for generators
        - delete tabular_modification
        - delete modification for tabular modifications
     */
    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIdsInGroup() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        // removing only first tabular modification in the group
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId(), List.of(modifications.get(0).getLeft()));
        // It is actually (4, 0, 1, 7) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(4, 0, 0, 0);
        assertEquals(4, modificationRepository.count()); // then second tabular still exists (and its sub-modifications)
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIdsInGroup2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();

        reset();
        // removing only 3 first tabular modifications in the group
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId(), modifications.subList(0, 3).stream().map(Pair::getLeft).toList());
        // It is actually (8, 0, 1, 21) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(8, 0, 0, 0);
        assertEquals(31, modificationRepository.count()); // then last tabular still exists (and its sub-modifications)
    }

    /*
    DELETE /v1/network-modifications SQL requests analysis

    Given an example with 2 tabular modifications having 1000 modifications each

    - 1 select to find modifications of this group
    - 2 select to retrieve tabular sub-modifications IDs (1 per tabular modification)
    - 14 deletes (7 per tabular modification):
        - delete reactive_capability_curve_points
        - delete free_property
        - delete generator_modification
        - delete tabular_modifications_modification
        - delete modification for generators
        - delete tabular_modification
        - delete modification for tabular modifications
     */

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIds() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());

        reset();
        // remove duplicates
        ApiUtils.deleteNetworkModifications(mockMvc, idsMapping.values().stream().toList());
        // It is actually (5, 0, 0, 14) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(5, 0, 0, 0);
        assertEquals(6, modificationRepository.count()); // source Modifications not removed
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIds2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        List<UUID> sourceModifications = modifications.stream().map(Pair::getLeft).toList();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, sourceModifications);

        reset();
        // remove duplicates
        ApiUtils.deleteNetworkModifications(mockMvc, idsMapping.values().stream().toList());
        // It is actually (9, 0, 0, 28) because deletes made in the native query are not counted
        TestUtils.assertRequestsCount(9, 0, 0, 0);
        assertEquals(48, modificationRepository.count()); // source Modifications not removed
    }

    @Test
    void testAllModificationsHaveFailed() throws Exception {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(-300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxP(new AttributeModification<>(-300., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxP(new AttributeModification<>(-300., OperationType.SET)).build()
        );
        ModificationInfos modificationInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .build();
        String modificationToCreateJson = getJsonBody(modificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();
        assertLogMessage("Tabular modification: No generators have been modified", "network.modification.tabular.modification.error", reportService);
    }

    @Test
    void testUnsupportedTabularModificationType() throws Exception {
        List<ModificationInfos> modifications = List.of(
                StaticVarCompensatorCreationInfos.builder()
                        .stashed(false)
                        .equipmentId("idStaticVarCompensator1")
                        .equipmentName("nameStaticVarCompensator1")
                        .voltageLevelId("v2")
                        .busOrBusbarSectionId("1B")
                        .connectionName("top")
                        .connectionDirection(ConnectablePosition.Direction.TOP)
                        .maxSusceptance(224.0)
                        .minSusceptance(200.0)
                        .maxQAtNominalV(null)
                        .minQAtNominalV(null)
                        .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
                        .voltageSetpoint(120.0)
                        .reactivePowerSetpoint(300.0)
                        .voltageRegulationType(VoltageRegulationType.LOCAL)
                        .standbyAutomatonOn(false)
                        .properties(List.of(FreePropertyInfos.builder().name("PROPERTY_NAME").value("PROPERTY_VALUE").build()))
                        .build()
        );
        ModificationInfos tabularInfos = TabularModificationInfos.builder()
                .modificationType(ModificationType.STATIC_VAR_COMPENSATOR_CREATION)
                .modifications(modifications)
                .build();
        String tabularModificationJson = getJsonBody(tabularInfos, null);

        // creation
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(tabularModificationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        NetworkModificationsResult result = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(result);
        assertEquals(1, result.modificationUuids().size());
        UUID modifId = result.modificationUuids().get(0);

        // try to get via the group
        UnsupportedOperationException exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
        assertEquals("No sub-modifications loading for modification type: STATIC_VAR_COMPENSATOR_CREATION", exception.getMessage());

        // try to get via id
        exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.getModificationInfo(modifId)
        );
        assertEquals("No sub-modifications loading for modification type: STATIC_VAR_COMPENSATOR_CREATION", exception.getMessage());

        // try to update
        exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.updateModification(modifId, tabularInfos)
        );
        // deletion error because we try to remove the sub-modifications before updating them
        assertEquals("No sub-modifications deletion for modification type: STATIC_VAR_COMPENSATOR_CREATION", exception.getMessage());

        // try to delete
        List<UUID> ids = List.of(modifId);
        exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.deleteModifications(TEST_GROUP_ID, ids)
        );
        assertEquals("No modification full deletion for type: STATIC_VAR_COMPENSATOR_CREATION", exception.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.GENERATOR_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.GENERATOR_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
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

    private static List<ModificationInfos> createGeneratorModificationList(int qty) {
        return IntStream.range(0, qty)
            .mapToObj(i ->
                (ModificationInfos) GeneratorModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .maxP(new AttributeModification<>(300., OperationType.SET))
                    .properties(List.of(
                        ModificationCreation.getFreeProperty(),
                        ModificationCreation.getFreeProperty("test", "value")))
                    .reactiveCapabilityCurvePoints(List.of(
                        ReactiveCapabilityCurvePointsInfos.builder().p(10.).build(),
                            ReactiveCapabilityCurvePointsInfos.builder().maxQ(17.).build(),
                        ReactiveCapabilityCurvePointsInfos.builder().minQ(5.).maxQ(5.).p(5.).build()))
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

    private static void assertTabularModificationsEquals(List<ModificationInfos> expectedModifications, List<ModificationInfos> modificationInfos) {
        assertThat(expectedModifications)
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(modificationInfos);
    }

    private static void assertTabularModificationsEquals(ModificationInfos expectedModificationInfos, ModificationInfos modificationInfos) {
        assertThat(expectedModificationInfos)
            .usingRecursiveComparison()
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(modificationInfos);
    }
}
