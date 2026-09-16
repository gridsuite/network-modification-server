/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularPropertyInfos;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.TabularPropertyRepository;
import org.gridsuite.modification.server.utils.ApiUtils;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.assertions.DTOAssert;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
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
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * SQL request count tests dedicated to tabular modifications: they check that the number of SQL requests issued to
 * read/copy/duplicate/delete a tabular modification does not depend on the number of its sub-modifications (i.e. the
 * JPA N+1 problem is correctly solved), and that no sub-modification / tabular property row is left orphaned after a
 * delete.
 *
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularGeneratorModificationsTest extends AbstractNetworkModificationTest {
    @Autowired
    private ModificationRepository modificationRepository;

    @Autowired
    private TabularPropertyRepository tabularPropertyRepository;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxP(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(true).build()))
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
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .stashed(false)
                .build();
    }

    @Test
    void testSqlRequestsCountOnGetModification() throws Exception {
        Pair<UUID, ModificationInfos> tabularWith1Modification = createTabularGeneratorModification(1);
        reset();
        ModificationInfos tabularWith1ModificationInfos = ApiUtils.getModification(mockMvc, tabularWith1Modification.getLeft()); // Getting one tabular modification with one sub-modification
        assertSelectCount(6);
        assertTabularModificationsEquals(tabularWith1Modification.getRight(), tabularWith1ModificationInfos);

        Pair<UUID, ModificationInfos> tabularWith3Modification = createTabularGeneratorModification(3);
        reset();
        ModificationInfos tabularWith3ModificationInfos = ApiUtils.getModification(mockMvc, tabularWith3Modification.getLeft()); // Getting one tabular modification with three sub-modifications
        assertSelectCount(6);
        assertTabularModificationsEquals(tabularWith3Modification.getRight(), tabularWith3ModificationInfos);
    }

    @Test
    void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        List<ModificationInfos> tabularModifications = ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(10);
        assertTabularModificationsEquals(modifications.stream().map(Pair::getRight).toList(), tabularModifications);
    }

    @Test
    void testSqlRequestsCountOnPostGroups() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId(), targetGroupUuid);
        TestUtils.assertRequestsCount(14, 9, 2, 0);
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPostGroups2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId(), targetGroupUuid);
        TestUtils.assertRequestsCount(22, 9, 2, 0);
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPutGroupsDuplications() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), targetGroupUuid, getNetworkUuid());
        TestUtils.assertRequestsCount(14, 9, 2, 0);
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPutGroupsDuplications2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), targetGroupUuid, getNetworkUuid());
        TestUtils.assertRequestsCount(22, 9, 2, 0);
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPutGroupsWithCopy() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, targetGroupUuid, modifications.stream().map(Pair::getLeft).toList(), getNetworkUuid());
        TestUtils.assertRequestsCount(13, 9, 2, 0);
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPutGroupsWithCopy2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        UUID targetGroupUuid = UUID.randomUUID();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, targetGroupUuid, modifications.stream().map(Pair::getLeft).toList(), getNetworkUuid());
        TestUtils.assertRequestsCount(21, 9, 2, 0);
        assertTabularModificationsEquals(modifications, targetGroupUuid);
    }

    @Test
    void testSqlRequestsCountOnPostNetworkModificationsDuplicate() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(10, 8, 2, 0);
        assertTabularModificationsEquals(modifications, idsMapping);
    }

    @Test
    void testSqlRequestsCountOnPostNetworkModificationsDuplicate2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();

        reset();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(18, 8, 2, 0);
        assertTabularModificationsEquals(modifications, idsMapping);
    }

    @Test
    void testSqlRequestsCountOnDeleteGroup() throws Exception {
        createFewTabularModifications();
        assertEquals(2, tabularPropertyRepository.count());

        reset();
        ApiUtils.deleteGroup(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(7, 0, 1, 1);
        assertEquals(0, modificationRepository.count());
        assertEquals(0, tabularPropertyRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteGroup2() throws Exception {
        createMoreTabularModifications();

        reset();
        ApiUtils.deleteGroup(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(11, 0, 1, 1);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteStashedInGroup() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        ApiUtils.stashNetworkModifications(mockMvc, modifications.stream().map(Pair::getLeft).toList());

        reset();
        ApiUtils.deleteStashedInGroup(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(6, 0, 1, 0);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteStashedInGroup2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();
        ApiUtils.stashNetworkModifications(mockMvc, modifications.stream().map(Pair::getLeft).toList());

        reset();
        ApiUtils.deleteStashedInGroup(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(10, 0, 1, 0);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsInGroup() throws Exception {
        createFewTabularModifications();

        reset();
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(6, 0, 1, 0);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsInGroup2() throws Exception {
        createMoreTabularModifications();

        reset();
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(10, 0, 1, 0);
        assertEquals(0, modificationRepository.count());
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIdsInGroup() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();

        reset();
        // removing only first tabular modification in the group
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId(), List.of(modifications.get(0).getLeft()));
        TestUtils.assertRequestsCount(4, 0, 1, 0);
        assertEquals(4, modificationRepository.count()); // then second tabular still exists (and its sub-modifications)
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIdsInGroup2() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createMoreTabularModifications();

        reset();
        // removing only 3 first tabular modifications in the group
        ApiUtils.deleteNetworkModificationsInGroup(mockMvc, getGroupId(), modifications.subList(0, 3).stream().map(Pair::getLeft).toList());
        TestUtils.assertRequestsCount(8, 0, 1, 0);
        assertEquals(31, modificationRepository.count()); // then last tabular still exists (and its sub-modifications)
    }

    @Test
    void testSqlRequestsCountOnDeleteNetworkModificationsByIds() throws Exception {
        List<Pair<UUID, ModificationInfos>> modifications = createFewTabularModifications();
        Map<UUID, UUID> idsMapping = ApiUtils.postNetworkModificationsDuplicate(mockMvc, modifications.stream().map(Pair::getLeft).toList());

        reset();
        // remove duplicates
        ApiUtils.deleteNetworkModifications(mockMvc, idsMapping.values().stream().toList());
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
        TestUtils.assertRequestsCount(9, 0, 0, 0);
        assertEquals(48, modificationRepository.count()); // source Modifications not removed
    }

    private UUID getNetworkUuid() {
        return ((NetworkImpl) getNetwork()).getUuid();
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
            .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
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
            .usingRecursiveComparison(DTOAssert.getRecursiveConfiguration(false))
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(modificationInfos);
    }

    private static void assertTabularModificationsEquals(ModificationInfos expectedModificationInfos, ModificationInfos modificationInfos) {
        assertThat(expectedModificationInfos)
            .usingRecursiveComparison(DTOAssert.getRecursiveConfiguration(false))
            .ignoringFields("uuid", "date", "modifications.uuid", "modifications.date")
            .isEqualTo(modificationInfos);
    }
}