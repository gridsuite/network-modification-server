/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.elementary.ElementaryModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.MatcherElementaryModificationInfos;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
public class ModificationRepositoryTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");

    @Autowired
    private ModificationGroupRepository modificationGroupRepository;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Before
    public void setUp() {
        modificationRepository.deleteAll();

        SQLStatementCountValidator.reset();
    }

    @Test
    public void test() {
        assertEquals(List.of(), this.modificationRepository.getModificationGroupsUuids());
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_NETWORK_ID)
        );

        var stringModifEntity = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        var boolModifEntity = modificationRepository.createElementaryModification("id2", "attribute", true);
        var intModifEntity = modificationRepository.createElementaryModification("id3", "attribute", 1);
        var floatModifEntity = modificationRepository.createElementaryModification("id4", "attribute", 2F);
        var doubleModifEntity = modificationRepository.createElementaryModification("id5", "attribute", 3D);

        modificationRepository.saveModifications(TEST_NETWORK_ID, List.of(stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity));

        List<ModificationInfos> modificationEntities = modificationRepository.getModifications(TEST_NETWORK_ID);
        assertEquals(5, modificationEntities.size());
        // Order is also checked
        assertThat(modificationRepository.getElementaryModification(TEST_NETWORK_ID, modificationEntities.get(0).getUuid()),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(stringModifEntity.toElementaryModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_NETWORK_ID, modificationEntities.get(1).getUuid()),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(boolModifEntity.toElementaryModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_NETWORK_ID, modificationEntities.get(2).getUuid()),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(intModifEntity.toElementaryModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_NETWORK_ID, modificationEntities.get(3).getUuid()),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(floatModifEntity.toElementaryModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_NETWORK_ID, modificationEntities.get(4).getUuid()),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(doubleModifEntity.toElementaryModificationInfos()));

        assertEquals(5, modificationRepository.getModifications(TEST_NETWORK_ID).size());
        assertEquals(List.of(TEST_NETWORK_ID), this.modificationRepository.getModificationGroupsUuids());

        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of());
        assertEquals(5, modificationRepository.getModifications(TEST_NETWORK_ID).size());
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(stringModifEntity.getId(), boolModifEntity.getId()));
        assertEquals(3, modificationRepository.getModifications(TEST_NETWORK_ID).size());

        modificationRepository.deleteModificationGroup(TEST_NETWORK_ID);
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_NETWORK_ID)
        );
    }

    @Test
    public void testCreateModificationGroupQueryCount() {
        modificationGroupRepository.save(new ModificationGroupEntity(TEST_NETWORK_ID));

        // No select
        assertRequestsCount(0, 1, 0, 0);
    }

    @Test
    public void testCreateModificationQueryCount() {
        var modifEntity1 = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        var modifEntity2 = modificationRepository.createElementaryModification("id2", "attribute", "foo");
        modificationRepository.saveModifications(TEST_NETWORK_ID, List.of(modifEntity1, modifEntity2));

        assertRequestsCount(1, 5, 0, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        var modifEntity1 = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        var modifEntity2 = modificationRepository.createElementaryModification("id2", "attribute", "foo");
        var modifEntity3 = modificationRepository.createElementaryModification("id3", "attribute", "foo");
        modificationRepository.saveModifications(TEST_NETWORK_ID, List.of(modifEntity1, modifEntity2, modifEntity3));

        SQLStatementCountValidator.reset();
        modificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getModifications(TEST_NETWORK_ID);
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getElementaryModification(TEST_NETWORK_ID, modifEntity1.getId());
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getElementaryModification(TEST_NETWORK_ID, TEST_NETWORK_ID)
        );
    }

    @Test
    public void testDeleteModificationQueryCount() {
        ElementaryModificationEntity<String> modifEntity1 = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        ElementaryModificationEntity<String> modifEntity2 = modificationRepository.createElementaryModification("id2", "attribute", "foo");
        modificationRepository.saveModifications(TEST_NETWORK_ID, List.of(modifEntity1, modifEntity2));

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(modifEntity1.getId()));
        assertRequestsCount(1, 0, 0, 2);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_NETWORK_ID);
        assertRequestsCount(2, 0, 0, 3);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(modifEntity2.getId()));
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent group modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.deleteModificationGroup(TEST_NETWORK_ID)
        );
    }

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }
}
