/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.ModificationReferenceData;
import org.gridsuite.modification.server.dto.PermissionType;
import org.gridsuite.modification.server.dto.ReferenceAttributes;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.test.web.client.ResponseActions;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClient;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.service.DirectoryService.HEADER_USER_ID;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.http.HttpMethod.*;
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

    @Test
    void testCreateElementReference() {
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

        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/" + elementUuid + "/references";
        directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(POST))
                .andExpect(header(HEADER_USER_ID, userId))
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.referenceId").value(referenceUuid.toString()))
                .andRespond(withSuccess());

        directoryService.createElementReference(elementUuid, referenceAttributes, userId);

        directoryServer.verify();
    }

    @Test
    void testRemoveElementReference() {
        UUID sharedElementUuid = UUID.randomUUID();
        UUID referenceUuid = UUID.randomUUID();
        String userId = "userId";

        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/" + sharedElementUuid + "/references/" + referenceUuid;
        directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(DELETE))
                .andExpect(header(HEADER_USER_ID, userId))
                .andRespond(withSuccess());

        directoryService.removeElementReference(sharedElementUuid, referenceUuid, userId);

        directoryServer.verify();
    }

    @Test
    void testRecreateReferences() {
        UUID nodeContainerUuid = UUID.randomUUID();
        UUID studyRootContainerUuid = UUID.randomUUID();
        String userId = "userId";

        UUID modification1Uuid = UUID.randomUUID();
        UUID referencedId1 = UUID.randomUUID();

        UUID modification2Uuid = UUID.randomUUID();
        UUID referencedId2 = UUID.randomUUID();

        List<ModificationReferenceData> referencesData = List.of(
                new ModificationReferenceData(modification1Uuid, referencedId1, UUID.randomUUID()),
                new ModificationReferenceData(modification2Uuid, referencedId2, null)
        );

        // First reference: inside composite (containerId != null)
        String expectedUrl1 = DIRECTORY_SERVER_BASE_URI + "/v1/elements/" + referencedId1 + "/references";
        directoryServer.expect(requestTo(expectedUrl1))
                .andExpect(method(POST))
                .andExpect(header(HEADER_USER_ID, userId))
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.referenceId").value(modification1Uuid.toString()))
                .andExpect(jsonPath("$.referenceType").value(ReferenceAttributes.ReferenceType.STUDY_NODE_NETWORK_MODIFICATION.toString()))
                .andRespond(withSuccess());

        // Second reference: at root level (containerId == null)
        String expectedUrl2 = DIRECTORY_SERVER_BASE_URI + "/v1/elements/" + referencedId2 + "/references";
        directoryServer.expect(requestTo(expectedUrl2))
                .andExpect(method(POST))
                .andExpect(header(HEADER_USER_ID, userId))
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.referenceId").value(modification2Uuid.toString()))
                .andExpect(jsonPath("$.referenceType").value(ReferenceAttributes.ReferenceType.STUDY_NODE.toString()))
                .andRespond(withSuccess());

        directoryService.recreateReferences(nodeContainerUuid, studyRootContainerUuid, userId, referencesData);

        directoryServer.verify();
    }

    @Test
    void testCheckPermission() {
        UUID firstElementUuid = UUID.randomUUID();
        UUID secondElementUuid = UUID.randomUUID();
        String userId = "userId";

        expectPermissionCheck(firstElementUuid, secondElementUuid, userId).andRespond(withSuccess());

        directoryService.checkPermission(List.of(firstElementUuid, secondElementUuid), userId, PermissionType.WRITE);

        directoryServer.verify();
    }

    @Test
    void testCheckPermissionThrowsWhenTheUserIsNotAllowed() {
        UUID firstElementUuid = UUID.randomUUID();
        UUID secondElementUuid = UUID.randomUUID();
        String userId = "userId";

        expectPermissionCheck(firstElementUuid, secondElementUuid, userId).andRespond(withForbiddenRequest());

        assertThrows(HttpClientErrorException.Forbidden.class,
                () -> directoryService.checkPermission(List.of(firstElementUuid, secondElementUuid), userId, PermissionType.WRITE));

        directoryServer.verify();
    }

    private ResponseActions expectPermissionCheck(UUID firstElementUuid, UUID secondElementUuid, String userId) {
        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/authorized"
                + "?ids=" + firstElementUuid + "&ids=" + secondElementUuid + "&accessType=WRITE";
        return directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(GET))
                .andExpect(header(HEADER_USER_ID, userId));
    }
}
