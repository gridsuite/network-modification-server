/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularTwoWindingsTransformerModificationsTest extends AbstractTabularModificationTest {
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
                .stashed(false)
                .build();
    }

    protected TwoWindingsTransformerModificationInfos buildOneModification(String equipmentId, Double seriesResistance) {
        return TwoWindingsTransformerModificationInfos.builder().equipmentId(equipmentId)
                .seriesResistance(new AttributeModification<>(seriesResistance, OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(0.0, getNetwork().getTwoWindingsTransformer("trf1").getR(), 0.001);
        assertEquals(1.0, getNetwork().getTwoWindingsTransformer("trf2").getR(), 0.001);
        assertLogMessage("TWO_WINDINGS_TRANSFORMER_NOT_FOUND : Two windings transformer with ID 'unknownTwt' does not exist in the network", ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name() + "1", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(2.0, getNetwork().getTwoWindingsTransformer("trf1").getR(), 0.001);
        assertEquals(2.0, getNetwork().getTwoWindingsTransformer("trf2").getR(), 0.001);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name(), updatedValues.get("tabularModificationType"));
    }

    @Override
    protected UUID createTabularModification(int qty) {
        ModificationInfos tabularModification = TabularModificationInfos.builder()
            .modificationType(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION)
            .modifications(createTwoWindingsTransformerModificationList(qty))
            .build();
        return saveModification(tabularModification);
    }

    private List<ModificationInfos> createTwoWindingsTransformerModificationList(int qty) {
        List<ModificationInfos> modifications = new ArrayList<>();
        for (int i = 0; i < qty; i++) {
            modifications.add(
                TwoWindingsTransformerModificationInfos.builder()
                    .equipmentId(UUID.randomUUID().toString())
                    .properties(List.of(ModificationCreation.getFreeProperty()))
                    .phaseTapChanger(
                        PhaseTapChangerModificationInfos.builder()
                            .regulationMode(AttributeModification.toAttributeModification(PhaseTapChanger.RegulationMode.FIXED_TAP, OperationType.SET))
                            .regulationValue(AttributeModification.toAttributeModification(1., OperationType.SET))
                            .build()
                    )
                    .build()
            );
        }
        return modifications;
    }
}
