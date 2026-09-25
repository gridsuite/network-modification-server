/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.PermissionType;
import org.gridsuite.modification.server.dto.ReferenceAttributes;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.test.web.client.ResponseActions;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.RestClientException;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.service.DirectoryService.HEADER_USER_ID;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.http.HttpMethod.GET;
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

    @AfterEach
    void tearDown() {
        directoryServer.verify();
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
    }

    @Test
    void testCheckPermission() {
        UUID firstElementUuid = UUID.randomUUID();
        UUID secondElementUuid = UUID.randomUUID();
        String userId = "userId";

        expectPermissionCheck(firstElementUuid, secondElementUuid, userId).andRespond(withSuccess());

        directoryService.checkPermission(List.of(firstElementUuid, secondElementUuid), userId, PermissionType.WRITE);
    }

    @Test
    void testCheckPermissionThrowsWhenTheUserIsNotAllowed() {
        UUID firstElementUuid = UUID.randomUUID();
        UUID secondElementUuid = UUID.randomUUID();
        String userId = "userId";

        expectPermissionCheck(firstElementUuid, secondElementUuid, userId).andRespond(withForbiddenRequest());

        assertThrows(HttpClientErrorException.Forbidden.class,
                () -> directoryService.checkPermission(List.of(firstElementUuid, secondElementUuid), userId, PermissionType.WRITE));
    }

    @Test
    void testGetElementsPermissions() {
        UUID readOnlyElementUuid = UUID.randomUUID();
        UUID writableElementUuid = UUID.randomUUID();
        String userId = "userId";

        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/permissions"
                + "?ids=" + readOnlyElementUuid + "&ids=" + writableElementUuid;
        directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(GET))
                .andExpect(header(HEADER_USER_ID, userId))
                .andRespond(withSuccess("{\"" + readOnlyElementUuid + "\":\"READ\",\"" + writableElementUuid + "\":\"WRITE\"}",
                        MediaType.APPLICATION_JSON));

        assertThat(directoryService.getElementsPermissions(List.of(readOnlyElementUuid, writableElementUuid), userId))
                .containsExactlyInAnyOrderEntriesOf(Map.of(readOnlyElementUuid, PermissionType.READ, writableElementUuid, PermissionType.WRITE));
    }

    @Test
    void testGetElementsPermissionsAsksNothingWithoutElement() {
        assertThat(directoryService.getElementsPermissions(Set.of(), "userId")).isEmpty();
    }

    @Test
    void testGetElementsPermissionsThrowsWhenTheDirectoryFails() {
        UUID elementUuid = UUID.randomUUID();
        String userId = "userId";

        directoryServer.expect(requestTo(DIRECTORY_SERVER_BASE_URI + "/v1/elements/permissions?ids=" + elementUuid))
                .andExpect(method(GET))
                .andRespond(withServerError());

        assertThrows(RestClientException.class, () -> directoryService.getElementsPermissions(List.of(elementUuid), userId));
    }

    private ResponseActions expectPermissionCheck(UUID firstElementUuid, UUID secondElementUuid, String userId) {
        String expectedUrl = DIRECTORY_SERVER_BASE_URI + "/v1/elements/authorized"
                + "?ids=" + firstElementUuid + "&ids=" + secondElementUuid + "&accessType=WRITE";
        return directoryServer.expect(requestTo(expectedUrl))
                .andExpect(method(GET))
                .andExpect(header(HEADER_USER_ID, userId));
    }
}
