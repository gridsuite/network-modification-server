/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ApiUtils;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.testcontainers.shaded.org.apache.commons.lang3.tuple.Pair;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularTwoWindingsTransformerModificationsTest extends AbstractNetworkModificationTest {
    @Autowired
    private ModificationRepository modificationRepository;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                buildOneModification("trf1", 0.0),
                buildOneModification("trf2", 1.0),
                buildOneModification("unknownTwt", 1.0)
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(true).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                buildOneModification("trf1", 3.0),
                buildOneModification("trf2", 4.0)
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .stashed(false)
                .build();
    }

    protected TwoWindingsTransformerModificationInfos buildOneModification(String equipmentId, Double seriesResistance) {
        return TwoWindingsTransformerModificationInfos.builder().equipmentId(equipmentId)
                .r(new AttributeModification<>(seriesResistance, OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(0.0, getNetwork().getTwoWindingsTransformer("trf1").getR(), 0.001);
        assertEquals(1.0, getNetwork().getTwoWindingsTransformer("trf2").getR(), 0.001);
        assertLogMessage("TWO_WINDINGS_TRANSFORMER_NOT_FOUND : Two windings transformer 'unknownTwt' : it does not exist in the network", "network.modification.tabular.modification.exception", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(2.0, getNetwork().getTwoWindingsTransformer("trf1").getR(), 0.001);
        assertEquals(2.0, getNetwork().getTwoWindingsTransformer("trf2").getR(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }

    @Test
    void testNoEntityLeftAfterCreationDeletion() throws Exception {
        List<Pair<UUID, ModificationInfos>> infos = createFewTabularModifications();
        // update first created tabular
        networkModificationRepository.updateModification(infos.getFirst().getLeft(), buildModificationUpdate());
        // delete
        ApiUtils.deleteGroup(mockMvc, getGroupId());
        assertEquals(0, modificationRepository.count());
    }

    private List<ModificationInfos> createTwtModificationList(int qty) {
        return IntStream.range(0, qty)
                .mapToObj(i ->
                        (ModificationInfos) TwoWindingsTransformerModificationInfos.builder().equipmentId(UUID.randomUUID().toString())
                                .x(new AttributeModification<>(1., OperationType.SET))
                                .g(new AttributeModification<>(1., OperationType.SET))
                                .operationalLimitsGroup1(TabularLineModificationsTest.buildOperationalLimitsGroupDefaultModification())
                                .operationalLimitsGroup2(TabularLineModificationsTest.buildOperationalLimitsGroupDefaultModification())
                                .properties(List.of(
                                        ModificationCreation.getFreeProperty(),
                                        ModificationCreation.getFreeProperty("test", "value")))
                                .build())
                .toList();
    }

    private List<Pair<UUID, ModificationInfos>> createFewTabularModifications() {
        Pair<UUID, ModificationInfos> tabular1 = createTabularTwtModification(2);
        Pair<UUID, ModificationInfos> tabular2 = createTabularTwtModification(1);
        return List.of(tabular1, tabular2);
    }

    private Pair<UUID, ModificationInfos> createTabularTwtModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
                .modificationType(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION)
                .modifications(createTwtModificationList(qty))
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .build();
        UUID uuid = saveModification(tabularModification);
        tabularModification.setUuid(uuid);
        return Pair.of(uuid, tabularModification);
    }
}
