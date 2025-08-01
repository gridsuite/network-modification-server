/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TwoSides;
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
 * @author Anis Touri <anis.touri at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularLineModificationsTest extends AbstractNetworkModificationTest {
    @Autowired
    private ModificationRepository modificationRepository;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                LineModificationInfos.builder().equipmentId("line1").r(new AttributeModification<>(10., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line2").x(new AttributeModification<>(20., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").g1(new AttributeModification<>(30., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").b1(new AttributeModification<>(40., OperationType.SET))
                        .operationalLimitsGroup1(buildOperationalLimitsGroupDefaultModification())
                        .operationalLimitsGroup2(buildOperationalLimitsGroupDefaultModification())
                        .build(),
                LineModificationInfos.builder().equipmentId("unknownLine").b2(new AttributeModification<>(60., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(true).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                LineModificationInfos.builder().equipmentId("line1").r(new AttributeModification<>(1., OperationType.SET))
                        .operationalLimitsGroup1(buildOperationalLimitsGroupDefaultModification())
                        .operationalLimitsGroup2(buildOperationalLimitsGroupDefaultModification())
                        .build(),
                LineModificationInfos.builder().equipmentId("line2").r(new AttributeModification<>(2., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").g1(new AttributeModification<>(3., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("line3").b1(new AttributeModification<>(4., OperationType.SET)).build(),
                LineModificationInfos.builder().equipmentId("unknownLine").b2(new AttributeModification<>(50., OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(modifications)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(10., getNetwork().getLine("line1").getR(), 0.001);
        assertEquals(20., getNetwork().getLine("line2").getX(), 0.001);
        assertEquals(30., getNetwork().getLine("line3").getG1(), 0.001);
        assertEquals(40., getNetwork().getLine("line3").getB1(), 0.001);
        assertLogMessage("LINE_NOT_FOUND : Line 'unknownLine' : does not exist in network", "network.modification.tabular.modification.exception", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(1., getNetwork().getLine("line1").getR(), 0.001);
        assertEquals(5., getNetwork().getLine("line2").getX(), 0.001);
        assertEquals(5.5, getNetwork().getLine("line3").getG1(), 0.001);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LINE_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.LINE_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
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

    private List<ModificationInfos> createLineModificationList(int qty) {
        return IntStream.range(0, qty)
            .mapToObj(i ->
                (ModificationInfos) LineModificationInfos.builder().equipmentId(UUID.randomUUID().toString())
                    .r(new AttributeModification<>(1., OperationType.SET))
                    .equipmentName(new AttributeModification<>("NAME", OperationType.SET))
                    .operationalLimitsGroup1(buildOperationalLimitsGroupDefaultModification())
                    .operationalLimitsGroup2(buildOperationalLimitsGroupDefaultModification())
                    .properties(List.of(
                            ModificationCreation.getFreeProperty(),
                            ModificationCreation.getFreeProperty("test", "value")))
                    .build())
            .toList();
    }

    private List<Pair<UUID, ModificationInfos>> createFewTabularModifications() {
        Pair<UUID, ModificationInfos> tabular1 = createTabularLineModification(1);
        Pair<UUID, ModificationInfos> tabular2 = createTabularLineModification(3);
        return List.of(tabular1, tabular2);
    }

    private Pair<UUID, ModificationInfos> createTabularLineModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
                .modificationType(ModificationType.LINE_MODIFICATION)
                .modifications(createLineModificationList(qty))
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .build();
        UUID uuid = saveModification(tabularModification);
        tabularModification.setUuid(uuid);
        return Pair.of(uuid, tabularModification);
    }

    public static List<OperationalLimitsGroupModificationInfos> buildOperationalLimitsGroupDefaultModification() {
        return List.of(buildOperationalLimitsGroupDefaultModification(TwoSides.ONE), buildOperationalLimitsGroupDefaultModification(TwoSides.TWO));
    }

    public static OperationalLimitsGroupModificationInfos buildOperationalLimitsGroupDefaultModification(TwoSides side) {
        return OperationalLimitsGroupModificationInfos.builder()
                .id("testName")
                .side(side.name())
                .modificationType(OperationalLimitsGroupModificationType.ADDED)
                .temporaryLimitsModificationType(TemporaryLimitModificationType.ADDED)
                .currentLimits(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(1200.)
                        .temporaryLimits(List.of(
                                CurrentTemporaryLimitModificationInfos.builder()
                                        .modificationType(TemporaryLimitModificationType.ADDED)
                                        .name("testLimit")
                                        .acceptableDuration(2)
                                        .value(10.)
                                        .build()
                        )).build())
                .build();
    }
}
