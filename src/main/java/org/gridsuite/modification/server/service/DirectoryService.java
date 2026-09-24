/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.PermissionType;
import org.gridsuite.modification.server.dto.ReferenceAttributes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@Service
public class DirectoryService {
    private static final String DIRECTORY_API_VERSION = "v1";
    private static final String DELIMITER = "/";
    public static final String HEADER_USER_ID = "userId";

    @Setter
    @Getter
    private static String directoryServerBaseUri;
    private final RestClient restClient;

    public DirectoryService(@Value("${gridsuite.services.directory-server.base-uri:http://directory-server/}") String directoryServerBaseUri,
                         RestClient restClient) {
        setDirectoryServerBaseUri(directoryServerBaseUri);
        this.restClient = restClient;
    }

    /**
     * updates a reference from a shared modification in directory server so it points to a new container.
     * the row to update is identified by elementUuid (the referenced shared element) and referenceUuid (the modification-reference itself)
     * @param elementUuid uuid of the referenced shared element in the directory-server
     * @param referenceAttributes new attributes of the modification-reference including its own referenceUuid which doesn't change
     * @param userId id of the user who moves the reference
     */
    public void updateElementReference(@NonNull UUID elementUuid, @NonNull ReferenceAttributes referenceAttributes, @NonNull String userId) {
        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references/{referenceUuid}")
                .buildAndExpand(elementUuid, referenceAttributes.getReferenceId())
                .toUriString();

        restClient.put()
                .uri(getDirectoryServerBaseUri() + path).header(HEADER_USER_ID, userId)
                .contentType(MediaType.APPLICATION_JSON)
                .body(referenceAttributes)
                .retrieve()
                .toBodilessEntity();
    }

    public void createElementReference(@NonNull UUID elementUuid, @NonNull ReferenceAttributes referenceAttributes, @NonNull String userId) {
        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references")
                .buildAndExpand(elementUuid)
                .toUriString();

        restClient.put()
                .uri(getDirectoryServerBaseUri() + path)
                .header(HEADER_USER_ID, userId)
                .contentType(MediaType.APPLICATION_JSON)
                .body(referenceAttributes)
                .retrieve()
                .toBodilessEntity();
    }

    /**
     * Checks that the user holds the given permission on every given element, and throws otherwise.
     * @param elementUuids uuids of the elements in the directory-server
     * @param userId id of the user the permission is checked for
     * @param permissionType the permission the user must hold
     */
    public void checkPermission(@NonNull Collection<UUID> elementUuids, @NonNull String userId, @NonNull PermissionType permissionType) {
        var path = UriComponentsBuilder.fromPath(DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/authorized")
                .queryParam("ids", elementUuids)
                .queryParam("accessType", permissionType)
                .buildAndExpand()
                .toUriString();

        restClient.get()
                .uri(getDirectoryServerBaseUri() + path)
                .header(HEADER_USER_ID, userId)
                .retrieve()
                .toBodilessEntity();
    }

    /**
     * Asks what the user may do with each of the given elements. The uuids travel in the query string, so a batch
     * far larger than a container holds would overrun the maximum length of a request line.
     * @param elementUuids uuids of the elements in the directory-server
     * @param userId id of the user the permissions are read for
     * @return the strongest permission held on each element, an element held no permission at all on and an element
     * unknown to the directory-server being left out
     */
    public Map<UUID, PermissionType> getElementsPermissions(@NonNull Collection<UUID> elementUuids, @NonNull String userId) {
        if (elementUuids.isEmpty()) {
            return Map.of();
        }
        var path = UriComponentsBuilder.fromPath(DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/permissions")
                .queryParam("ids", elementUuids)
                .buildAndExpand()
                .toUriString();

        Map<UUID, PermissionType> permissions = restClient.get()
                .uri(getDirectoryServerBaseUri() + path)
                .header(HEADER_USER_ID, userId)
                .retrieve()
                .body(new ParameterizedTypeReference<>() { });
        return permissions == null ? Map.of() : permissions;
    }
}
