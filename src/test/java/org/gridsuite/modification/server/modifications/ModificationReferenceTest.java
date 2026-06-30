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
}
