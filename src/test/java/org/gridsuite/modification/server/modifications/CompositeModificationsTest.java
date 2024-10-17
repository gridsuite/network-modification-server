/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.CompositeModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
class CompositeModificationsTest extends AbstractNetworkModificationTest {

    @MockBean
    private NetworkModificationApplicator networkModificationApplicator;

    @BeforeEach
    void specificSetUp() {
        // Currently we never apply composite modifications (apply mocked)
        NetworkModificationResult networkModificationResultMock = NetworkModificationResult.builder()
                .applicationStatus(NetworkModificationResult.ApplicationStatus.ALL_OK)
                .lastGroupApplicationStatus(NetworkModificationResult.ApplicationStatus.ALL_OK)
                .networkImpacts(List.of())
                .build();
        when(networkModificationApplicator.applyModifications(any(), any(), any(ReportInfos.class)))
                .then((Answer<NetworkModificationResult>) invocation -> networkModificationResultMock);
        when(networkModificationApplicator.applyModifications(any(), any(), any(ReportInfos.class)))
                .then((Answer<NetworkModificationResult>) invocation -> networkModificationResultMock);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, false);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD",
                        "v1"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1"));
        return CompositeModificationInfos.builder()
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return buildModification();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
    }

    @Test
    void testCheckSqlRequestsCount() throws Exception {
        UUID modificationUuid = saveModification(buildModification());
        SQLStatementCountValidator.reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        assertSelectCount(7);
        SQLStatementCountValidator.reset();

        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
                .andExpect(status().isOk());
        SQLStatementCountValidator.assertSelectCount(8);
    }
}
