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

import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.MatcherElementaryModificationInfos;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
public class ModificationRepositoryTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Test
    public void testElementaryModification() {
        ElementaryModificationEntity stringModifEntity = new ElementaryModificationEntity("id1", Set.of(), new StringAttributeEntity("attribute", "foo"));
        ElementaryModificationEntity boolModifEntity = new ElementaryModificationEntity("id2", Set.of(), new BooleanAttributeEntity("attribute", true));
        ElementaryModificationEntity intModifEntity = new ElementaryModificationEntity("id3", Set.of(), new IntegerAttributeEntity("attribute", 1));
        ElementaryModificationEntity floatModifEntity = new ElementaryModificationEntity("id4", Set.of(), new FloatAttributeEntity("attribute", 2));
        ElementaryModificationEntity doubleModifEntity = new ElementaryModificationEntity("id5", Set.of(), new DoubleAttributeEntity("attribute", 3));
        modificationRepository.insertElementaryModification(TEST_NETWORK_ID, stringModifEntity);
        modificationRepository.insertElementaryModification(TEST_NETWORK_ID, boolModifEntity);
        modificationRepository.insertElementaryModification(TEST_NETWORK_ID, intModifEntity);
        modificationRepository.insertElementaryModification(TEST_NETWORK_ID, floatModifEntity);
        modificationRepository.insertElementaryModification(TEST_NETWORK_ID, doubleModifEntity);

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

        assertEquals(5, modificationRepository.getElementaryModifications(TEST_NETWORK_ID).size());
        assertEquals(5, modificationRepository.getElementaryModifications().size());

        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of());
        assertEquals(5, modificationRepository.getElementaryModifications(TEST_NETWORK_ID).size());
        modificationRepository.deleteModifications(TEST_NETWORK_ID, Set.of(stringModifEntity.getUuid(), boolModifEntity.getUuid()));
        assertEquals(3, modificationRepository.getElementaryModifications(TEST_NETWORK_ID).size());
        assertEquals(3, modificationRepository.getElementaryModifications().size());

        modificationRepository.deleteModificationGroup(TEST_NETWORK_ID);
        assertFalse(modificationRepository.getModificationGroup(TEST_NETWORK_ID).isPresent());
        assertEquals(0, modificationRepository.getElementaryModifications().size());
    }

}
