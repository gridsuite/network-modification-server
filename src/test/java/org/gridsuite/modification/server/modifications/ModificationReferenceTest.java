/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Tag("IntegrationTest")
class ModificationReferenceTest extends AbstractNetworkModificationTest {

    @Autowired
    protected ModificationRepository modificationRepository;

    @Autowired
    protected NetworkModificationRepository networkModificationRepository;

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

    @Test
    void testCompositeToModificationInfosFillsMissingDisplayMessage() {
        ModificationInfos compositeInfo = buildCompositeModification();
        ModificationEntity compositeEntity = modificationRepository.save(ModificationEntity.fromDTO(compositeInfo));

        CompositeModificationInfos result = (CompositeModificationInfos) compositeEntity.toModificationInfos();

        ModificationInfos resultChild = result.getModificationsInfos().get(0);
        assertEquals(ModificationType.LOAD_CREATION.name(), resultChild.getMessageType());
        assertNotNull(resultChild.getMessageValues());
    }

    @Test
    void testCompositeToModificationInfosPreservesExistingDisplayMessage() {
        // child already carries a messageType/messageValues: withDisplayMessage() must
        // leave them untouched (the "non-null" branch of each if).
        ModificationInfos child = ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED);
        child.setMessageType("LOAD_CREATION");
        child.setMessageValues("{\"equipmentId\":\"idLoad\"}");
        CompositeModificationInfos compositeInfo = CompositeModificationInfos.builder()
                .name("composite")
                .modificationsInfos(List.of(child))
                .stashed(false)
                .build();
        ModificationEntity compositeEntity = modificationRepository.save(ModificationEntity.fromDTO(compositeInfo));

        CompositeModificationInfos result = (CompositeModificationInfos) compositeEntity.toModificationInfos();

        ModificationInfos resultChild = result.getModificationsInfos().get(0);
        assertEquals(ModificationType.LOAD_CREATION.name(), resultChild.getMessageType());
        assertEquals("{\"equipmentId\":\"idLoad\"}", resultChild.getMessageValues());
    }

    @Test
    void testGetModificationReferenceInfoFillsChildDisplayMessageFromRepository() {
        ModificationInfos compositeInfo = buildCompositeModification();
        ModificationEntity compositeEntity = modificationRepository.save(ModificationEntity.fromDTO(compositeInfo));

        ModificationInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.BASIC)
                .referenceId(compositeEntity.getId())
                .referenceInfos(compositeEntity.toModificationInfos())
                .stashed(false)
                .activated(true)
                .build();
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(UUID.randomUUID(), List.of(referenceInfos));
        UUID referenceUuid = saved.get(0).getUuid();

        ModificationInfos fetched = networkModificationRepository.getModificationInfo(referenceUuid);

        assertInstanceOf(ModificationReferenceInfos.class, fetched);
        ModificationInfos refInfos = ((ModificationReferenceInfos) fetched).getReferenceInfos();
        assertInstanceOf(CompositeModificationInfos.class, refInfos);
        ModificationInfos fetchedChild = ((CompositeModificationInfos) refInfos).getModificationsInfos().get(0);
        assertEquals(ModificationType.LOAD_CREATION.name(), fetchedChild.getMessageType());
        assertNotNull(fetchedChild.getMessageValues());
    }

    @Test
    void testGetModificationReferenceInfoPreservesExistingChildDisplayMessageFromRepository() {
        // Same DB-reload path as above, but the child already has custom messageType/messageValues:
        // the null-guards in loadModificationReference() must leave them untouched, not overwrite
        // them with the computed fallback. Covers the false branch of the two new ifs.
        ModificationInfos child = ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED);
        child.setMessageType(ModificationType.LOAD_CREATION.name());
        child.setMessageValues("{\"equipmentId\":\"idLoad\"}");
        CompositeModificationInfos compositeInfo = CompositeModificationInfos.builder()
                .name("composite")
                .modificationsInfos(List.of(child))
                .stashed(false)
                .build();
        ModificationEntity compositeEntity = modificationRepository.save(ModificationEntity.fromDTO(compositeInfo));

        ModificationInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.BASIC)
                .referenceId(compositeEntity.getId())
                .referenceInfos(compositeEntity.toModificationInfos())
                .stashed(false)
                .activated(true)
                .build();
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(UUID.randomUUID(), List.of(referenceInfos));
        UUID referenceUuid = saved.get(0).getUuid();

        ModificationInfos fetched = networkModificationRepository.getModificationInfo(referenceUuid);

        ModificationInfos refInfos = ((ModificationReferenceInfos) fetched).getReferenceInfos();
        ModificationInfos fetchedChild = ((CompositeModificationInfos) refInfos).getModificationsInfos().get(0);
        assertEquals(ModificationType.LOAD_CREATION.name(), fetchedChild.getMessageType());
        assertEquals("{\"equipmentId\":\"idLoad\"}", fetchedChild.getMessageValues());
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

    @Test
    void testGetModificationReferenceToNonComposite() {
        ModificationInfos load = ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED);
        ModificationEntity loadEntity = modificationRepository.save(ModificationEntity.fromDTO(load));

        ModificationInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.BASIC)
                .referenceId(loadEntity.getId())
                .referenceInfos(loadEntity.toModificationInfos())
                .stashed(false)
                .activated(true)
                .build();
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(UUID.randomUUID(), List.of(referenceInfos));

        ModificationInfos fetched = networkModificationRepository.getModificationInfo(saved.get(0).getUuid());

        assertInstanceOf(ModificationReferenceInfos.class, fetched);
        assertNotNull(((ModificationReferenceInfos) fetched).getReferenceInfos());
    }

    @Test
    void testFillDisplayMessageKeepsExistingValues() {
        ModificationInfos child = ModificationCreation.getCreationLoad(
                "v1",
                "idLoad",
                "nameLoad",
                "1.1",
                LoadType.UNDEFINED
        );

        child.setMessageType(null);
        child.setMessageValues(null);

        ModificationInfos result = CompositeModificationEntity.fillDisplayMessage(child);

        assertEquals("LOAD_CREATION", result.getMessageType());
        assertEquals("{\"equipmentId\":\"idLoad\"}", result.getMessageValues());
    }

    @Test
    void shouldReturnEmptyJsonWhenWriteValuesFails() {
        ModificationInfos child = new ModificationInfos();

        Map<String, Object> map = new HashMap<>();
        map.put("self", map);

        child.setMessageValues(map.toString());
        String result = NetworkModificationRepository.writeValuesQuietly(new ObjectMapper(), child);

        assertEquals("{}", result);
    }
}
