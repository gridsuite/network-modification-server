/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Tag("IntegrationTest")
class ModificationReferenceTest extends AbstractNetworkModificationTest {

    @Autowired
    protected ModificationRepository modificationRepository;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        ModificationInfos compositeInfo = buildCompositeModification();
        ModificationEntity compositeEntity = modificationRepository.save(ModificationEntity.fromDTO(compositeInfo));
        ModificationInfos referenceInfos = compositeEntity.toModificationInfos();

        return ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.BASIC)
                .referenceId(compositeEntity.getId())
                .referenceInfos(referenceInfos)
                .stashed(false)
                .activated(true)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return buildModification();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLoad("idLoad"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLoad("idLoad"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    private ModificationInfos buildCompositeModification() {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED)
        );
        return CompositeModificationInfos.builder()
                .name("composite")
                .modificationsInfos(modifications)
                .stashed(false)
                .build();
    }

    private ModificationInfos buildCompositeModificationWithTwoChildren() {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationLoad("v1", "idLoad2", "nameLoad2", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationLoad("v1", "idLoad3", "nameLoad3", "1.1", LoadType.UNDEFINED)
        );
        return CompositeModificationInfos.builder()
                .name("composite-with-two-children")
                .modificationsInfos(modifications)
                .stashed(false)
                .build();
    }

    @Test
    void testReferenceToCompositeFillsChildrenDisplayMessages() {
        ModificationInfos compositeInfo = buildCompositeModificationWithTwoChildren();
        ModificationEntity compositeEntity = modificationRepository.save(ModificationEntity.fromDTO(compositeInfo));

        ModificationInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.BASIC)
                .referenceId(compositeEntity.getId())
                .referenceInfos(compositeEntity.toModificationInfos())
                .stashed(false)
                .activated(true)
                .build();
        saveModification(referenceInfos);

        ModificationInfos loaded = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertInstanceOf(ModificationReferenceInfos.class, loaded);
        ModificationInfos loadedReferenceInfos = ((ModificationReferenceInfos) loaded).getReferenceInfos();
        assertInstanceOf(CompositeModificationInfos.class, loadedReferenceInfos);

        List<ModificationInfos> children = ((CompositeModificationInfos) loadedReferenceInfos).getModificationsInfos();
        assertEquals(2, children.size());
        children.forEach(child -> {
            assertEquals(ModificationType.LOAD_CREATION.name(), child.getMessageType());
            assertNotNull(child.getMessageValues());
            assertDoesNotThrow(() -> mapper.readTree(child.getMessageValues()));
        });
    }

    @Test
    void testCompositeToModificationInfosOnlyFillsMissingDisplayMessage() {
        ModificationInfos childWithoutMessage = ModificationCreation.getCreationLoad("v1", "idLoad4", "nameLoad4", "1.1", LoadType.UNDEFINED);
        ModificationInfos childWithMessage = ModificationCreation.getCreationLoad("v1", "idLoad5", "nameLoad5", "1.1", LoadType.UNDEFINED);
        childWithMessage.setMessageType("CUSTOM_MESSAGE_TYPE");
        childWithMessage.setMessageValues("{\"custom\":true}");

        CompositeModificationInfos compositeInfo = CompositeModificationInfos.builder()
                .name("composite-with-mixed-children")
                .modificationsInfos(List.of(childWithoutMessage, childWithMessage))
                .stashed(false)
                .build();
        CompositeModificationEntity compositeEntity = (CompositeModificationEntity) ModificationEntity.fromDTO(compositeInfo);

        List<ModificationInfos> children = compositeEntity.toModificationInfos().getModificationsInfos();

        ModificationInfos filled = children.getFirst();
        assertEquals(ModificationType.LOAD_CREATION.name(), filled.getMessageType());
        assertNotNull(filled.getMessageValues());
        assertDoesNotThrow(() -> mapper.readTree(filled.getMessageValues()));

        ModificationInfos untouched = children.get(1);
        assertEquals("LOAD_CREATION", untouched.getMessageType());
        assertEquals("{\"equipmentId\":\"idLoad5\"}", untouched.getMessageValues());
    }
}