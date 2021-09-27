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

import com.powsybl.iidm.network.LoadType;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.elementary.CreateLoadEntity;
import org.gridsuite.modification.server.entities.elementary.ElementaryModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.MatcherElementaryAttributeModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLoadCreationInfos;
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

    private static final UUID TEST_GROUP_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");

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
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID)
        );

        var nullModifEntity = modificationRepository.createElementaryModification("id0", "attribute", null);
        var stringModifEntity = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        var boolModifEntity = modificationRepository.createElementaryModification("id2", "attribute", true);
        var intModifEntity = modificationRepository.createElementaryModification("id3", "attribute", 1);
        var floatModifEntity = modificationRepository.createElementaryModification("id4", "attribute", 2F);
        var doubleModifEntity = modificationRepository.createElementaryModification("id5", "attribute", 3D);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(nullModifEntity, stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity));

        List<ModificationInfos> modificationEntities = modificationRepository.getModifications(TEST_GROUP_ID);
        assertEquals(6, modificationEntities.size());
        // Order is also checked
        assertThat(modificationRepository.getElementaryModification(TEST_GROUP_ID, modificationEntities.get(0).getUuid()),
                MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos(nullModifEntity.toElementaryAttributeModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_GROUP_ID, modificationEntities.get(1).getUuid()),
                MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos(stringModifEntity.toElementaryAttributeModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_GROUP_ID, modificationEntities.get(2).getUuid()),
                MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos(boolModifEntity.toElementaryAttributeModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_GROUP_ID, modificationEntities.get(3).getUuid()),
                MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos(intModifEntity.toElementaryAttributeModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_GROUP_ID, modificationEntities.get(4).getUuid()),
                MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos(floatModifEntity.toElementaryAttributeModificationInfos()));
        assertThat(modificationRepository.getElementaryModification(TEST_GROUP_ID, modificationEntities.get(5).getUuid()),
                MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos(doubleModifEntity.toElementaryAttributeModificationInfos()));

        assertEquals(6, modificationRepository.getModifications(TEST_GROUP_ID).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of());
        assertEquals(6, modificationRepository.getModifications(TEST_GROUP_ID).size());
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(stringModifEntity.getId(), boolModifEntity.getId()));
        assertEquals(4, modificationRepository.getModifications(TEST_GROUP_ID).size());

        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID)
        );
    }

    @Test
    public void testCreateModificationGroupQueryCount() {
        modificationGroupRepository.save(new ModificationGroupEntity(TEST_GROUP_ID));

        // No select
        assertRequestsCount(0, 1, 0, 0);
    }

    @Test
    public void testCreateModificationQueryCount() {
        var modifEntity1 = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        var modifEntity2 = modificationRepository.createElementaryModification("id2", "attribute", "foo");
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        assertRequestsCount(1, 5, 0, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        var modifEntity1 = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        var modifEntity2 = modificationRepository.createElementaryModification("id2", "attribute", "foo");
        var modifEntity3 = modificationRepository.createElementaryModification("id3", "attribute", "foo");
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2, modifEntity3));

        SQLStatementCountValidator.reset();
        modificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getModifications(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getElementaryModification(TEST_GROUP_ID, modifEntity1.getId());
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getElementaryModification(TEST_GROUP_ID, TEST_GROUP_ID)
        );
    }

    @Test
    public void testDeleteModificationQueryCount() {
        ElementaryModificationEntity<String> modifEntity1 = modificationRepository.createElementaryModification("id1", "attribute", "foo");
        ElementaryModificationEntity<String> modifEntity2 = modificationRepository.createElementaryModification("id2", "attribute", "foo");
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(modifEntity1.getId()));
        assertRequestsCount(1, 0, 0, 2);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(modifEntity2.getId()));
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent group modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.deleteModificationGroup(TEST_GROUP_ID)
        );
    }

    @Test
    public void testLoadCreation() {
        var createLoadEntity1 = modificationRepository.createLoadEntity("idLoad1", "nameLoad1", LoadType.AUXILIARY, "vlId1", "busId1", 100.0, 20.0);
        var createLoadEntity2 = modificationRepository.createLoadEntity("idLoad2", "nameLoad2", LoadType.FICTITIOUS, "vlId2", "busId2", 80.0, 30.0);
        var createLoadEntity3 = modificationRepository.createLoadEntity("idLoad3", "nameLoad3", LoadType.UNDEFINED, "vlId3", "busId3", 50.0, 90.0);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLoadEntity1, createLoadEntity2, createLoadEntity3));
        assertRequestsCount(1, 7, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID);
        assertEquals(3, modificationInfos.size());

        assertThat(modificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((CreateLoadEntity) createLoadEntity1).toLoadCreationInfos()));
        assertThat(modificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((CreateLoadEntity) createLoadEntity2).toLoadCreationInfos()));
        assertThat(modificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((CreateLoadEntity) createLoadEntity3).toLoadCreationInfos()));

        assertEquals(3, modificationRepository.getModifications(TEST_GROUP_ID).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createLoadEntity2.getId(), createLoadEntity3.getId()));
        assertRequestsCount(1, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID)
        );
    }

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }
}
