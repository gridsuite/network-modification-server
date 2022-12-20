/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Network;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.junit.Before;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.UUID;

import static org.gridsuite.modification.server.utils.MatcherDeleteAttachingLineInfos.createMatcherDeleteAttachingLineInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteAttachingLineTest extends AbstractNetworkModificationTest {

    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkWithTeePoint.create(TEST_NETWORK_ID);
            return network;
        });

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
                .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
        SQLStatementCountValidator.reset();
    }

    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteAttachingLineInfos.builder()
                .type(ModificationType.DELETE_ATTACHING_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineId")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return DeleteAttachingLineInfos.builder()
                .type(ModificationType.DELETE_ATTACHING_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineIdEdited")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherDeleteAttachingLineInfos((DeleteAttachingLineInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("replacingLineId"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("replacingLineIdEdited"));
    }
}
