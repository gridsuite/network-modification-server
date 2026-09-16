/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.ReferenceAttributes;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.web.client.RestClient;

import java.util.UUID;

import static org.gridsuite.modification.server.service.DirectoryService.HEADER_USER_ID;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.*;
import static org.springframework.test.web.client.response.MockRestResponseCreators.*;

class DirectoryServiceTest {
    private static final String DIRECTORY_SERVER_BASE_URI = "http://directory-server-test";

    private MockRestServiceServer directoryServer;
    private DirectoryService directoryService;

    @BeforeEach
    void setUp() {
        RestClient.Builder restClientBuilder = RestClient.builder();
        directoryServer = MockRestServiceServer.bindTo(restClientBuilder).build();
        directoryService = new DirectoryService(DIRECTORY_SERVER_BASE_URI, restClientBuilder.build());
    }

    @Test
    void testUpdateElementReference() {
        UUID elementUuid = UUID.randomUUID();
        UUID referenceUuid = UUID.randomUUID();
        UUID rootContainerId = UUID.randomUUID();
        UUID containerId = UUID.randomUUID();
        String userId = "userId";
        ReferenceAttributes referenceAttributes = ReferenceAttributes.createReferenceAttributes(
                referenceUuid,
                rootContainerId,
                containerId,
                ReferenceAttributes.ReferenceType.STUDY_NODE
        );

        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/" + elementUuid + "/references/" + referenceUuid;
        directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(PUT))
                .andExpect(header(HEADER_USER_ID, userId))
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.referenceId").value(referenceUuid.toString()))
                .andRespond(withSuccess());

        directoryService.updateElementReference(elementUuid, referenceAttributes, userId);

        directoryServer.verify();
    }
}
