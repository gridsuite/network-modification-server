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
import org.gridsuite.modification.server.dto.ModificationReferenceData;
import org.gridsuite.modification.server.dto.ReferenceAttributes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
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

    private void updateElementsReferences(List<ModificationReferenceData> modificationReferences, UUID rootContainerId, UUID containerId,
                                          ReferenceAttributes.ReferenceType targetReferenceType, String userId) {
        modificationReferences.forEach(ref -> updateElementReference(
                ref.referencedId(),
                ReferenceAttributes.createReferenceAttributes(ref.modificationUuid(), rootContainerId, containerId, targetReferenceType), userId)
        );
    }

    /**
     * updates a reference from a shared modification in directory server so it points to a new container.
     * the row to update is identified by elementUuid (the referenced shared element) and referenceUuid (the modification-reference itself)
     * @param elementUuid uuid of the referenced shared element in the directory-server
     * @param referenceAttributes new attributes of the modification-reference including its own referenceUuid which doesn't change
     * @param userId id of the user who moves the reference
     */
    public void updateElementReference(@NonNull UUID elementUuid, @NonNull ReferenceAttributes referenceAttributes, String userId) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(HEADER_USER_ID, userId);
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<ReferenceAttributes> requestEntity = new HttpEntity<>(referenceAttributes, headers);

        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references/{referenceUuid}")
                .buildAndExpand(elementUuid, referenceAttributes.getReferenceId())
                .toUriString();

        restClient.put().uri(getDirectoryServerBaseUri() + path).body(requestEntity);
    }

    public void createElementReference(@NonNull UUID elementUuid, @NonNull ReferenceAttributes referenceAttributes, String userId) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(HEADER_USER_ID, userId);
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<ReferenceAttributes> requestEntity = new HttpEntity<>(referenceAttributes, headers);

        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references")
                .buildAndExpand(elementUuid)
                .toUriString();

        restClient.post().uri(getDirectoryServerBaseUri() + path).body(requestEntity);
    }
}
