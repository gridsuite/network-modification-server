/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.ElementAttributes;
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
import static org.springframework.http.HttpMethod.GET;
import static org.springframework.http.HttpMethod.PUT;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.*;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withForbiddenRequest;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess;

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
    void testUpdateElement() {
        UUID elementUuid = UUID.randomUUID();
        String userId = "userId";
        ElementAttributes elementAttributes = ElementAttributes.createElementAttributes(elementUuid, "name", "description");

        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/" + elementUuid;
        directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(PUT))
                .andExpect(header(HEADER_USER_ID, userId))
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andRespond(withSuccess());

        directoryService.updateElement(elementAttributes, userId);

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
                .andExpect(method(PUT))
                .andExpect(header(HEADER_USER_ID, userId))
                .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                .andExpect(jsonPath("$.referenceId").value(referenceUuid.toString()))
                .andRespond(withSuccess());

        directoryService.createElementReference(elementUuid, referenceAttributes, userId);

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
