/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.ApiUtils;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.Test;

import java.util.List;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public abstract class AbstractTabularModificationTest extends AbstractNetworkModificationTest {

    @Test
    public void testSqlRequestsCountOnGetModification() throws Exception {
        UUID tabularWith1ModificationUuid = createTabularModification(1);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith1ModificationUuid); // Getting one tabular modification with one sub-modification
        assertSelectCount(3);

        UUID tabularWith3ModificationUuid = createTabularModification(3);
        reset();
        ApiUtils.getModification(mockMvc, tabularWith3ModificationUuid); // Getting one tabular modification with three sub-modifications
        assertSelectCount(3);
    }

    @Test
    public void testSqlRequestsCountOnGetGroupModifications() throws Exception {
        createTabularModification(1);
        createTabularModification(3);

        reset();
        ApiUtils.getGroupModifications(mockMvc, getGroupId()); // Getting two tabular modifications with respectively one and three sub-modifications
        assertSelectCount(6);
    }

    /*
    POST /v1/groups SQL requests analysis

    First we select the modifications to copy:
    - 1 select on the modification_group (check if it exists)
    - 1 select to retrieve the modifications
    - 2 selects per tabular modification to get the ids of the sub-modifications and the sub-modifications themselves
    - 1 select on the modification_group to check if the new group exists

    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)

    Then modifications order is set:
    - 1 update in modification for orders (batchSize: number of tabular modifications)

    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)

    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPostGroups() throws Exception {
        createFewTabularModifications();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(7, getNumberOfInserts(), getNumberOfUpdates(), 0);
    }

    @Test
    public void testSqlRequestsCountOnPostGroups2() throws Exception {
        createMoreTabularModifications();

        reset();
        ApiUtils.postGroups(mockMvc, getGroupId());
        TestUtils.assertRequestsCount(11, getNumberOfInserts(), getNumberOfUpdates(), 0);
    }

    /*
    PUT /v1/groups/{groupUuid}/duplications SQL requests analysis

    First we select the modifications to copy:
    - 1 select on the modification_group (check if it exists)
    - 1 select to retrieve the modifications
    - 2 selects per tabular modification to get the ids of the sub-modifications and the sub-modifications themselves
    - 1 select on the modification_group to check if the new group exists

    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)

    Then modifications order is set:
    - 1 update in modification for orders (batchSize: number of tabular modifications)

    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)

    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPutGroupsDuplications() throws Exception {
        createFewTabularModifications();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), getNetworkId());
        TestUtils.assertRequestsCount(7, getNumberOfInserts(), getNumberOfUpdates(), 0);
    }

    @Test
    public void testSqlRequestsCountOnPutGroupsDuplications2() throws Exception {
        createMoreTabularModifications();

        reset();
        ApiUtils.putGroupsDuplications(mockMvc, getGroupId(), getNetworkId());
        TestUtils.assertRequestsCount(11, getNumberOfInserts(), getNumberOfUpdates(), 0);
    }

    /*
    PUT /v1/groups/{groupUuid}?action=COPY SQL requests analysis

    First we select the modifications to copy:
    - 1 select to retrieve the modifications
    - 2 selects per tabular modification to get the ids of the sub-modifications and the sub-modifications themselves
    - 1 select on the modification_group to check if the new group exists

    Then we insert the new modifications in the new group:
    - 1 insert in modification_group to create the new group
    - 1 insert in modification for tabular modifications (batchSize: number of tabular modifications)
    - 1 insert in tabular_modification (batchSize: number of tabular modifications)
    - 1 insert in modification for sub-modifications (batchSize: number of sub-modifications)
    - 1 insert in sub-modification table (batchSize: number of sub-modifications)
    - (optional) 1 insert in sub-modification relation tables (batchSize: number of sub-modifications)

    Then modifications order is set:
    - 1 update in modification for orders (batchSize: number of tabular modifications)

    Then relation between tabular modifications and sub-modifications are set:
    - 1 insert in tabular_modification_modifications for the relation (batchSize: number of sub-modifications)

    (optional) Then order of sub-modifications relations are set:
    - 1 update in sub-modifications relation for the relation (batchSize: number of sub-modifications)
     */
    @Test
    public void testSqlRequestsCountOnPutGroupsWithCopy() throws Exception {
        List<UUID> ids = createFewTabularModifications();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, ids, getNetworkId());
        TestUtils.assertRequestsCount(6, getNumberOfInserts(), getNumberOfUpdates(), 0);
    }

    @Test
    public void testSqlRequestsCountOnPutGroupsWithCopy2() throws Exception {
        List<UUID> ids = createMoreTabularModifications();

        reset();
        ApiUtils.putGroupsWithCopy(mockMvc, ids, getNetworkId());
        TestUtils.assertRequestsCount(10, getNumberOfInserts(), getNumberOfUpdates(), 0);
    }

    private List<UUID> createFewTabularModifications() {
        UUID tabularUuid1 = createTabularModification(1);
        UUID tabularUuid2 = createTabularModification(3);

        return List.of(tabularUuid1, tabularUuid2);
    }

    private List<UUID> createMoreTabularModifications() {
        UUID tabularUuid1 = createTabularModification(1);
        UUID tabularUuid2 = createTabularModification(3);
        UUID tabularUuid3 = createTabularModification(10);
        UUID tabularUuid4 = createTabularModification(30);

        return List.of(tabularUuid1, tabularUuid2, tabularUuid3, tabularUuid4);
    }

    // Some sub-modifications are related to other tables, for example FreeProperties, then it requires additional inserts
    protected int getAdditionalSubModificationRelatedTables() {
        return 0;
    }

    // Some sub-modifications are related to other ordered tables, for example FreeProperties, then it requires additional updates
    protected int getAdditionalSubModificationOrderedRelatedTables() {
        return 0;
    }

    protected abstract UUID createTabularModification(int qty);

    private int getNumberOfInserts() {
        return 6 + getAdditionalSubModificationRelatedTables();
    }

    private int getNumberOfUpdates() {
        return 1 + getAdditionalSubModificationOrderedRelatedTables();
    }
}
