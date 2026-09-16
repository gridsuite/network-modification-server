/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularPropertyInfos;
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
import java.util.UUID;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Checks that no sub-modification row is left orphaned after a tabular two windings transformer modification is
 * created, updated, then its group is deleted.
 *
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
                buildOneModification("trf2", 1.0)
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
                .enableOLGModification(true)
                .build();
    }

    @Test
    void testNoEntityLeftAfterCreationUpdateDeletion() throws Exception {
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
                                .enableOLGModification(true)
                                .operationalLimitsGroups(TabularLineModificationsTest.buildOperationalLimitsGroupDefaultModification())
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
