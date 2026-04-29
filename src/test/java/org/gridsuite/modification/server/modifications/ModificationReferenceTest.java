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
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
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
        ModificationInfos loadInfo = ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED);
        ModificationEntity loadEntity = modificationRepository.save(ModificationEntity.fromDTO(loadInfo));
        ModificationInfos loadMetadataInfo = modificationRepository.findBaseDataByIdIn(List.of(loadEntity.getId())).getFirst().toModificationInfos();

        return ModificationReferenceInfos.builder()
            .messageType(loadMetadataInfo.getMessageType())
            .messageValues(loadMetadataInfo.getMessageValues())
            .referenceType(ModificationReferenceInfos.Type.SAMPLE)
            .referenceId(loadMetadataInfo.getUuid())
            .referenceInfos(loadInfo)
            .stashed(false)
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
        assertEquals(ModificationType.LOAD_CREATION.name(), modificationInfos.getMessageType());
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.LOAD_CREATION.name(), modificationInfos.getMessageType());
    }
}
