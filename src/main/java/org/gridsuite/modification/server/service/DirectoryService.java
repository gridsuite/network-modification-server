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
import org.gridsuite.modification.server.dto.PermissionType;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Collection;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationController.HEADER_USER_ID;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@Service
public class DirectoryService {
    private static final String DIRECTORY_API_VERSION = "v1";
    private static final String DELIMITER = "/";

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
}
