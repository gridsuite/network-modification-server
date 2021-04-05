/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import java.util.Set;
import java.util.UUID;

import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
public class ModificationServiceTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Before
    public void setUp() {
        //modificationRepository.deleteAll();

        SQLStatementCountValidator.reset();
    }

    @Test
    public void testCreateModificationQueryCount() {
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id1", Set.of(), "attribute", "foo");

        assertRequestsCount(2, 5, 0, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id1", Set.of(), "attribute", "foo");
        modificationRepository.createElementaryModification(TEST_NETWORK_ID, "id2", Set.of(), "attribute", "foo");

        SQLStatementCountValidator.reset();

        modificationRepository.getModifications(TEST_NETWORK_ID);

        assertRequestsCount(2, 0, 0, 0);
    }

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }

}
