/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.MatcherElementaryModificationInfos;
import org.gridsuite.modification.server.entities.ElementaryModificationEntity;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
public class ModificationRepositoryTest {

    @Autowired
    private ModificationRepository modificationRepository;

    @Test
    public void testElementaryModification() {
        ElementaryModificationEntity modificationEntity1 = new ElementaryModificationEntity("id1", "name1", "attribute", "value");
        ElementaryModificationEntity modificationEntity2 = new ElementaryModificationEntity("id2", "name2", "attribute", "value");
        modificationRepository.insert(modificationEntity1);
        modificationRepository.insert(modificationEntity2);

        List<ElementaryModificationEntity> elementaryModificationEntities = modificationRepository.getElementaryModifications();
        assertEquals(2, elementaryModificationEntities.size());
        assertThat(elementaryModificationEntities.get(0).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(modificationEntity1.toElementaryModificationInfos()));
        assertThat(elementaryModificationEntities.get(1).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(modificationEntity2.toElementaryModificationInfos()));

        elementaryModificationEntities = modificationRepository.getElementaryModifications("id1");
        assertEquals(1, elementaryModificationEntities.size());
        assertThat(elementaryModificationEntities.get(0).toElementaryModificationInfos(),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(modificationEntity1.toElementaryModificationInfos()));
    }

}
