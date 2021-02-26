/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.MatcherElementaryModificationInfos;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@AutoConfigureEmbeddedDatabase(provider = AutoConfigureEmbeddedDatabase.DatabaseProvider.ZONKY)
public class ModificationRepositoryTest {

    @Autowired
    private ModificationRepository modificationRepository;

    @Test
    public void testElementaryModification() {
        ElementaryModificationEntity stringModifEntity = new ElementaryModificationEntity("id5", Set.of(), new StringAttributeEntity("attribute", "foo"));
        ElementaryModificationEntity boolModifEntity = new ElementaryModificationEntity("id1", Set.of(), new BooleanAttributeEntity("attribute", true));
        ElementaryModificationEntity intModifEntity = new ElementaryModificationEntity("id2", Set.of(), new IntegerAttributeEntity("attribute", 1));
        ElementaryModificationEntity floatModifEntity = new ElementaryModificationEntity("id3", Set.of(), new FloatAttributeEntity("attribute", 2));
        ElementaryModificationEntity doubleModifEntity = new ElementaryModificationEntity("id4", Set.of(), new DoubleAttributeEntity("attribute", 3));
        modificationRepository.insertModification(stringModifEntity);
        modificationRepository.insertModification(boolModifEntity);
        modificationRepository.insertModification(intModifEntity);
        modificationRepository.insertModification(floatModifEntity);
        modificationRepository.insertModification(doubleModifEntity);

        List<ElementaryModificationEntity> elementaryModificationEntities = modificationRepository.getElementaryModifications();
        assertEquals(5, elementaryModificationEntities.size());
        // Order is also checked
        assertThat(elementaryModificationEntities.get(0).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(stringModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(1).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(boolModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(2).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(intModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(3).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(floatModifEntity.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(4).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(doubleModifEntity.toElementaryModificationInfos()));

        elementaryModificationEntities = modificationRepository.getElementaryModifications("id1");
        assertEquals(1, elementaryModificationEntities.size());
        assertThat(elementaryModificationEntities.get(0).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(boolModifEntity.toElementaryModificationInfos()));

        assertEquals(0, modificationRepository.getElementaryModifications(List.of()).size());
        assertEquals(1, modificationRepository.getElementaryModifications(List.of(stringModifEntity.getUuid())).size());
        elementaryModificationEntities = modificationRepository.getElementaryModifications(
                List.of(stringModifEntity.getUuid(),  boolModifEntity.getUuid(), intModifEntity.getUuid(),
                        floatModifEntity.getUuid(), doubleModifEntity.getUuid()
                )
        );
        assertEquals(5, elementaryModificationEntities.size());
    }

}
