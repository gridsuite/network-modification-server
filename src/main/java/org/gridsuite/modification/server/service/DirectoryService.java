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
import org.gridsuite.modification.server.dto.ElementAttributes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

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

    public void updateElement(@NonNull UUID elementUuid, @NonNull ElementAttributes elementAttributes, String userId) {
        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}")
                .buildAndExpand(elementUuid)
                .toUriString();

        restClient.put()
                .uri(getDirectoryServerBaseUri() + path)
                .contentType(MediaType.APPLICATION_JSON)
                .header(HEADER_USER_ID, userId)
                .body(elementAttributes)
                .retrieve()
                .toBodilessEntity();
    }
}
