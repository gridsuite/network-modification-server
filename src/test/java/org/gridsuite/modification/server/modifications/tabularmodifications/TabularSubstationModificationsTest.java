/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
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
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularSubstationModificationsTest extends AbstractNetworkModificationTest {
    public static final ModificationType MOFIFICATION_TYPE = ModificationType.SUBSTATION_MODIFICATION;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {

        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).substationCountry(new AttributeModification<>(Country.BE, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).substationCountry(new AttributeModification<>(Country.BE, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(MOFIFICATION_TYPE)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).substationCountry(new AttributeModification<>(Country.JP, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).substationCountry(new AttributeModification<>(Country.JP, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(MOFIFICATION_TYPE)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(Country.BE, getNetwork().getSubstation("s1").getCountry().orElse(Country.AF));
        assertEquals("s1", getNetwork().getSubstation("s1").getOptionalName().orElse("s2"));
        assertEquals(Country.BE, getNetwork().getSubstation("s2").getCountry().orElse(Country.AF));
        assertEquals("s2", getNetwork().getSubstation("s2").getOptionalName().orElse("s1"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(Country.FR, getNetwork().getSubstation("s1").getCountry().orElse(Country.BE));
        assertEquals("s1", getNetwork().getSubstation("s1").getOptionalName().orElse("s2"));
        assertEquals(Country.FR, getNetwork().getSubstation("s2").getCountry().orElse(Country.BE));
        assertEquals("s2", getNetwork().getSubstation("s2").getOptionalName().orElse("s1"));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(MOFIFICATION_TYPE.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(MOFIFICATION_TYPE.name(), updatedValues.get("tabularModificationType"));
    }

    @Test
    public void testSqlRequestsCountOnGetModification() throws Exception {
        UUID tabularWith1ModificationUuid = createTabularSubstationModification(1);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith1ModificationUuid); // Getting one tabular modification with one sub-modification
        assertSelectCount(3);

        UUID tabularWith3ModificationUuid = createTabularSubstationModification(3);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith3ModificationUuid); // Getting one tabular modification with three sub-modifications
        assertSelectCount(3);
    }

    @Test
    public void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        createTabularSubstationModification(1);
        createTabularSubstationModification(3);

        reset();
        ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(6);
    }

    private UUID createTabularSubstationModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.SUBSTATION_MODIFICATION)
            .modifications(createSubstationModificationList(qty))
            .build();
        return saveModification(tabularModification);
    }

    private List<ModificationInfos> createSubstationModificationList(int qty) {
        List<ModificationInfos> modifications = new ArrayList<>();
        for (int i = 0; i <= qty; i++) {
            modifications.add(
                SubstationModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .properties(List.of(ModificationCreation.getFreeProperty()))
                    .build()
            );
        }
        return modifications;
    }
}
