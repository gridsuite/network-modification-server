/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @Autowired
    private ModificationRepository modificationRepository;

    @BeforeEach
    void specificSetUp() {
        SQLStatementCountValidator.reset();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
            ModificationCreation.getCreationGenerator("v1", "idGeneratorComposite", "nameGenerator", "1.1", "v2load", "LOAD",
                "v1"),
            ModificationCreation.getCreationLoad("v1", "idLoadComposite", "nameLoad", "1.1", LoadType.UNDEFINED),
            CompositeModificationInfos.builder().name("battery composite")
                .modificationsInfos(List.of(ModificationCreation.getCreationBattery("v1", "idBatteryComposite", "nameBattry", "1.1")))
                .stashed(false).build()
        );
        return CompositeModificationInfos.builder()
            .name("composite")
            .modificationsInfos(modifications)
            .stashed(false)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return buildModification();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getGenerator("idGeneratorComposite"));
        assertNotNull(getNetwork().getLoad("idLoadComposite"));
        assertNotNull(getNetwork().getBattery("idBatteryComposite"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getGenerator("idGeneratorComposite"));
        assertNull(getNetwork().getLoad("idLoadComposite"));
        assertNull(getNetwork().getBattery("idBatteryComposite"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    @Test
    void testCheckSqlRequestsCount() throws Exception {
        UUID modificationUuid = saveModification(buildModification());

        SQLStatementCountValidator.reset();
        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
            .andReturn();
        assertSelectCount(8);

        SQLStatementCountValidator.reset();
        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
            .andExpect(status().isOk());
        SQLStatementCountValidator.assertSelectCount(9);
    }

    @Test
    void testDBLoadWithOptimization() {
        SubstationCreationInfos siteInfo1 = SubstationCreationInfos.builder().equipmentId("id1").equipmentName("site1").build();
        CompositeModificationInfos compositeInfo1 = CompositeModificationInfos.builder().name("composite1").modificationsInfos(List.of()).build();
        CompositeModificationInfos compositeInfo2 = CompositeModificationInfos.builder().name("composite2").modificationsInfos(List.of(siteInfo1, compositeInfo1)).build();
        CompositeModificationInfos compositeInfo3 = CompositeModificationInfos.builder().name("composite3").modificationsInfos(List.of(createTabularModification(), compositeInfo2)).build();
        CompositeModificationInfos compositeInfo41 = CompositeModificationInfos.builder().name("composite41").modificationsInfos(List.of(compositeInfo3)).build();
        // Use a random number of composites to test the optimization (no N+1 select)
        List<ModificationInfos> compositeInfos = IntStream.range(0, new Random().nextInt(10) + 1)
            .mapToObj(i -> CompositeModificationInfos.builder().name("composite5" + i).modificationsInfos(List.of()).build())
            .map(ModificationInfos.class::cast)
            .toList();
        CompositeModificationInfos compositeInfo42 = CompositeModificationInfos.builder().name("composite42").modificationsInfos(compositeInfos).build();

        CompositeModificationInfos compositeInfo = CompositeModificationInfos.builder().name("composite").modificationsInfos(List.of(compositeInfo41, compositeInfo42)).build();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(ModificationEntity.fromDTO(compositeInfo)));

        SQLStatementCountValidator.reset();
        List<ModificationInfos> modifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertRequestsCount(9, 0, 0, 0);

        SQLStatementCountValidator.reset();
        List<UUID> uuids = networkModificationRepository.findAllChildrenUuids(List.of(modifications.get(0).getUuid()));
        assertEquals(uuids.size(), uuids.stream().collect(Collectors.toSet()).size());
        assertEquals(8 + compositeInfos.size(), uuids.size());
    }

    @Test
    void testDBLoadOfCompositesAndReferencesByBatch() {
        // the request count must not depend on the number of composites and references to convert (no N+1 select):
        // the active modifications, the composites content, the shared composites and their content, the applicabilities
        // (the recursive CTE finding the nested composites starts with WITH, it is not counted as a select)
        assertActiveModificationsSelectCount(1, 5);
        assertActiveModificationsSelectCount(4, 5);
    }

    private void assertActiveModificationsSelectCount(int count, int expectedSelectCount) {
        UUID groupUuid = UUID.randomUUID();
        List<ModificationEntity> modifications = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            CompositeModificationInfos nestedComposite = CompositeModificationInfos.builder().name("nested" + i)
                .modificationsInfos(List.of(GroovyScriptInfos.builder().script("script" + i).build())).build();
            modifications.add(ModificationEntity.fromDTO(CompositeModificationInfos.builder().name("composite" + i)
                .modificationsInfos(List.of(nestedComposite)).stashed(false).build()));
            UUID sharedCompositeUuid = modificationRepository.save(ModificationEntity.fromDTO(CompositeModificationInfos.builder().name("shared" + i)
                .modificationsInfos(List.of(GroovyScriptInfos.builder().script("shared script" + i).build())).build())).getId();
            modifications.add(ModificationEntity.fromDTO(ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.BASIC).referenceId(sharedCompositeUuid).stashed(false).build()));
        }
        networkModificationRepository.saveModifications(groupUuid, modifications);

        SQLStatementCountValidator.reset();
        List<ModificationInfos> activeModifications = networkModificationRepository.getActiveModifications(groupUuid, "tag");
        assertRequestsCount(expectedSelectCount, 0, 0, 0);

        assertEquals(2 * count, activeModifications.size());
        activeModifications.stream()
            .filter(ModificationReferenceInfos.class::isInstance)
            .map(ModificationReferenceInfos.class::cast)
            .forEach(reference -> assertEquals(reference.getReferenceId(), reference.getReferenceInfos().getUuid()));
        activeModifications.forEach(modification -> assertEquals(Map.of(), modification.getApplicabilityByRootNetworkTag()));
    }

    @Test
    void testDBLoadOfNestedReferencesByBatch() {
        // the references are prefetched level by level, like the composites: the request count depends on how deep
        // they are nested, not on how many they are. The active modifications, then for each of the three levels (the
        // group composites, the shared composites they reference, the shared composites those reference) the content
        // of the composites and, but for the last one, the shared composites of the next level, and the applicabilities
        assertNestedReferencesSelectCount(1, 7);
        assertNestedReferencesSelectCount(4, 7);
    }

    private void assertNestedReferencesSelectCount(int count, int expectedSelectCount) {
        UUID groupUuid = UUID.randomUUID();
        List<ModificationEntity> modifications = new ArrayList<>();
        List<UUID> innerSharedUuids = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            UUID innerSharedUuid = modificationRepository.save(ModificationEntity.fromDTO(CompositeModificationInfos.builder().name("inner shared" + i)
                .modificationsInfos(List.of(GroovyScriptInfos.builder().script("inner script" + i).build())).build())).getId();
            UUID outerSharedUuid = modificationRepository.save(ModificationEntity.fromDTO(CompositeModificationInfos.builder().name("outer shared" + i)
                .modificationsInfos(List.of(referenceTo(innerSharedUuid))).build())).getId();
            modifications.add(ModificationEntity.fromDTO(CompositeModificationInfos.builder().name("composite" + i)
                .modificationsInfos(List.of(referenceTo(outerSharedUuid))).stashed(false).build()));
            innerSharedUuids.add(innerSharedUuid);
        }
        networkModificationRepository.saveModifications(groupUuid, modifications);

        SQLStatementCountValidator.reset();
        List<ModificationInfos> activeModifications = networkModificationRepository.getActiveModifications(groupUuid, "tag");
        assertRequestsCount(expectedSelectCount, 0, 0, 0);

        assertEquals(count, activeModifications.size());
        for (int i = 0; i < count; i++) {
            ModificationReferenceInfos outerReference = (ModificationReferenceInfos) ((CompositeModificationInfos) activeModifications.get(i)).getModificationsInfos().getFirst();
            ModificationReferenceInfos innerReference = (ModificationReferenceInfos) ((CompositeModificationInfos) outerReference.getReferenceInfos()).getModificationsInfos().getFirst();
            assertEquals(innerSharedUuids.get(i), innerReference.getReferenceInfos().getUuid());
        }
    }

    private static ModificationReferenceInfos referenceTo(UUID sharedUuid) {
        return ModificationReferenceInfos.builder().referenceType(ModificationReferenceInfos.Type.BASIC).referenceId(sharedUuid).stashed(false).build();
    }

    private TabularModificationInfos createTabularModification() {
        // Use a random number of generators to test the optimization (no N+1 select)
        List<ModificationInfos> generatorModifications = IntStream.range(0, new Random().nextInt(10) + 1)
            .mapToObj(i -> GeneratorModificationInfos.builder().equipmentId("generator" + i).maxP(new AttributeModification<>(500., OperationType.SET)).build())
            .map(ModificationInfos.class::cast)
            .toList();
        return
            TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(generatorModifications)
                .stashed(false)
                .build();
    }
}
