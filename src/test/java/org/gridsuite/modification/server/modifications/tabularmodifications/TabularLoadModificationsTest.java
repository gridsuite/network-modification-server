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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.junit.Assert.assertEquals;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularLoadModificationsTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                LoadModificationInfos.builder().equipmentId("v1load").q0(new AttributeModification<>(300., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v2load").q0(new AttributeModification<>(300., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v3load").q0(new AttributeModification<>(300., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LOAD_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                LoadModificationInfos.builder().equipmentId("v1load").q0(new AttributeModification<>(500., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v2load").q0(new AttributeModification<>(500., OperationType.SET)).build(),
                LoadModificationInfos.builder().equipmentId("v3load").q0(new AttributeModification<>(500., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LOAD_MODIFICATION)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(300., getNetwork().getLoad("v1load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v2load").getQ0(), 0.001);
        assertEquals(300., getNetwork().getLoad("v3load").getQ0(), 0.001);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(0., getNetwork().getLoad("v1load").getQ0(), 0.001);
        assertEquals(0., getNetwork().getLoad("v2load").getQ0(), 0.001);
        assertEquals(0., getNetwork().getLoad("v3load").getQ0(), 0.001);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.LOAD_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.LOAD_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }

    @Test
    public void testSqlRequestsCountOnGetModification() throws Exception {
        UUID tabularWith1ModificationUuid = createTabularLoadModification(1);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith1ModificationUuid); // Getting one tabular modification with one sub-modification
        assertSelectCount(3);

        UUID tabularWith3ModificationUuid = createTabularLoadModification(3);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith3ModificationUuid); // Getting one tabular modification with three sub-modifications
        assertSelectCount(3);
    }

    @Test
    public void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        createTabularLoadModification(1);
        createTabularLoadModification(3);

        reset();
        ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(6);
    }

    private UUID createTabularLoadModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.LOAD_MODIFICATION)
            .modifications(createLoadModificationList(qty))
            .build();
        return saveModification(tabularModification);
    }

    private List<ModificationInfos> createLoadModificationList(int qty) {
        List<ModificationInfos> modifications = new ArrayList<>();
        for (int i = 0; i <= qty; i++) {
            modifications.add(
                LoadModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .properties(List.of(ModificationCreation.getFreeProperty()))
                    .build()
            );
        }
        return modifications;
    }
}
