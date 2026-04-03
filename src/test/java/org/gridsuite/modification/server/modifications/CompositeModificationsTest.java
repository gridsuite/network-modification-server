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
import org.gridsuite.modification.server.dto.ModificationApplicationGroup;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @MockitoBean
    private NetworkModificationApplicator networkModificationApplicator;

    @BeforeEach
    void specificSetUp() {
        // Currently we never apply composite modifications (apply mocked)
        CompletableFuture<NetworkModificationResult> networkModificationResultMock = CompletableFuture.completedFuture(NetworkModificationResult.builder()
            .applicationStatus(NetworkModificationResult.ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(NetworkModificationResult.ApplicationStatus.ALL_OK)
            .networkImpacts(List.of())
            .build());
        when(networkModificationApplicator.applyModifications(any(ModificationApplicationGroup.class), any()))
            .then(invocation -> networkModificationResultMock);
        SQLStatementCountValidator.reset();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
            ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD",
                "v1"),
            ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
            ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1"));
        return CompositeModificationInfos.builder()
            .modifications(modifications)
            .stashed(false)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return buildModification();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
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

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
            .andReturn();
        SQLStatementCountValidator.reset();
        assertSelectCount(7);

        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
            .andExpect(status().isOk());
        SQLStatementCountValidator.assertSelectCount(8);
    }

    @Test
    void testDBLoadWithOptimization() {
        SubstationCreationInfos siteInfo1 = SubstationCreationInfos.builder().equipmentId("id1").equipmentName("site1").build();
        CompositeModificationInfos compositeInfo1 = CompositeModificationInfos.builder().name("composite1").modifications(List.of()).build();
        CompositeModificationInfos compositeInfo2 = CompositeModificationInfos.builder().name("composite2").modifications(List.of(siteInfo1, compositeInfo1)).build();
        CompositeModificationInfos compositeInfo3 = CompositeModificationInfos.builder().name("composite3").modifications(List.of(createTabularModification(), compositeInfo2)).build();
        CompositeModificationInfos compositeInfo41 = CompositeModificationInfos.builder().name("composite41").modifications(List.of(compositeInfo3)).build();
        CompositeModificationInfos compositeInfo42 = CompositeModificationInfos.builder().name("composite42").modifications(List.of()).build();

        CompositeModificationInfos compositeInfo = CompositeModificationInfos.builder().name("composite").modifications(List.of(compositeInfo41, compositeInfo42)).build();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(ModificationEntity.fromDTO(compositeInfo)));

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertRequestsCount(8, 0, 0, 0);
    }

    private TabularModificationInfos createTabularModification() {

        List<ModificationInfos> groupModifications = List.of(
            GeneratorModificationInfos.builder().equipmentId("generator1").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
            GeneratorModificationInfos.builder().equipmentId("generator2").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
            GeneratorModificationInfos.builder().equipmentId("generator3").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
            GeneratorModificationInfos.builder().equipmentId("generator4").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
            GeneratorModificationInfos.builder().equipmentId("generator5").maxP(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return
            TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(groupModifications)
                .stashed(false)
                .build();
    }
}
