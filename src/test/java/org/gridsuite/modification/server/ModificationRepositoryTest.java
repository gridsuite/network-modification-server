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
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.entities.elementary.ElementaryModificationEntity;
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
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getElementaryModifications(TEST_NETWORK_ID)
        );

        ElementaryModificationEntity<String> stringModifEntity = this.modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id1", Set.of(), "attribute", "foo");
        ElementaryModificationEntity<Boolean> boolModifEntity = this.modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id2", Set.of(), "attribute", true);
        ElementaryModificationEntity<Integer> intModifEntity = this.modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id3", Set.of(), "attribute", 1);
        ElementaryModificationEntity<Float> floatModifEntity = this.modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id4", Set.of(), "attribute", 2F);
        ElementaryModificationEntity<Double> doubleModifEntity = this.modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id5", Set.of(), "attribute", 3D);

        List<ElementaryModificationInfos> elementaryModificationEntities = modificationRepository.getElementaryModifications(TEST_NETWORK_ID);
        assertEquals(5, elementaryModificationEntities.size());
        // Order is also checked
        assertThat(elementaryModificationEntities.get(0),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(stringModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(1),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(boolModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(2),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(intModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(3),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(floatModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(4),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(doubleModifEntity.toElementaryModificationInfos()));

        assertEquals(5, modificationRepository.getModifications(TEST_NETWORK_ID).size());
        assertEquals(5, modificationRepository.getElementaryModifications(TEST_NETWORK_ID).size());
        assertEquals(List.of(TEST_NETWORK_ID), this.modificationRepository.getModificationGroupsUuids());

        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of());
        assertEquals(5, modificationRepository.getElementaryModifications(TEST_NETWORK_ID).size());
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(stringModifEntity.getUuid(), boolModifEntity.getUuid()));
        assertEquals(3, modificationRepository.getElementaryModifications(TEST_NETWORK_ID).size());

        modificationRepository.deleteModificationGroup(TEST_NETWORK_ID);
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getElementaryModifications(TEST_NETWORK_ID)
        );
    }

    @Test
    public void testCreateModificationQueryCount() {
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id1", Set.of(), "attribute", "foo");

        assertRequestsCount(2, 3, 0, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id1", Set.of(), "attribute", "foo");
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id2", Set.of(), "attribute", "foo");
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id3", Set.of(), "attribute", "foo");

        SQLStatementCountValidator.reset();
        modificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getModifications(TEST_NETWORK_ID);
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getElementaryModifications(TEST_NETWORK_ID);
        assertRequestsCount(2, 0, 0, 0);
    }

    @Test
    public void testDeleteModificationQueryCount() {
        ElementaryModificationEntity<String> modifEntity1 = modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id1", Set.of(), "attribute", "foo");
        ElementaryModificationEntity<String> modifEntity2 = modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id2", Set.of(), "attribute", "foo");

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(modifEntity1.getUuid()));
        assertRequestsCount(1, 0, 0, 2);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_NETWORK_ID);
        assertRequestsCount(2, 0, 0, 3);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(modifEntity2.getUuid()));
        assertRequestsCount(1, 0, 0, 0);
    }

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }
}
