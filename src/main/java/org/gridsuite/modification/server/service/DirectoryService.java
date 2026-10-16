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
import org.gridsuite.modification.server.dto.PermissionType;
import org.gridsuite.modification.server.dto.ReferenceAttributes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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
     * updates references from a shared modification in directory server in a single call so they point to a new container.
     * the rows to update are identified by elementUuid (the referenced shared element) and, for each entry, the referenceUuid
     * carried by its ReferenceAttributes (the modification-reference itself)
     * @param elementUuid uuid of the referenced shared element in the directory-server
     * @param referencesAttributes new attributes of the modification-references including their own referenceUuid which doesn't change
     * @param userId id of the user who moves the references
     */
    public void updateElementReferences(@NonNull UUID elementUuid, @NonNull List<ReferenceAttributes> referencesAttributes, @NonNull String userId) {
        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references")
                .buildAndExpand(elementUuid)
                .toUriString();

        restClient.put()
                .uri(getDirectoryServerBaseUri() + path).header(HEADER_USER_ID, userId)
                .contentType(MediaType.APPLICATION_JSON)
                .body(referencesAttributes)
                .retrieve()
                .toBodilessEntity();
    }

    public void createElementReferences(@NonNull UUID elementUuid, @NonNull List<ReferenceAttributes> referencesAttributes, @NonNull String userId) {
        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references")
                .buildAndExpand(elementUuid)
                .toUriString();

        restClient.post()
                .uri(getDirectoryServerBaseUri() + path)
                .header(HEADER_USER_ID, userId)
                .contentType(MediaType.APPLICATION_JSON)
                .body(referencesAttributes)
                .retrieve()
                .toBodilessEntity();
    }

    public void removeElementReferences(@NonNull List<ModificationReferenceData> referencesData, String userId) {
        // one call per referenced shared element, carrying all its references to remove
        Map<UUID, List<UUID>> referenceUuidsByReferencedId = new LinkedHashMap<>();
        for (ModificationReferenceData referenceData : referencesData) {
            referenceUuidsByReferencedId.computeIfAbsent(referenceData.referencedId(), k -> new ArrayList<>()).add(referenceData.modificationUuid());
        }
        for (Map.Entry<UUID, List<UUID>> entry : referenceUuidsByReferencedId.entrySet()) {
            removeElementReferences(entry.getKey(), entry.getValue(), userId);
        }
    }

    /**
     * remove references from the shared modification in directory server in a single call
     * @param referenceUuids uuids of the 'Modification references' to remove
     * @param userId id of the user who caused the unreferencing
     * @param sharedElementUuid uuid of the referenced shared element in the directory-server
     */
    private void removeElementReferences(UUID sharedElementUuid, List<UUID> referenceUuids, String userId) {
        Objects.requireNonNull(referenceUuids);
        Objects.requireNonNull(sharedElementUuid);

        var path = UriComponentsBuilder.fromPath(
                        DELIMITER + DIRECTORY_API_VERSION + DELIMITER + "elements/{elementUuid}/references")
                .buildAndExpand(sharedElementUuid)
                .toUriString();

        restClient.method(HttpMethod.DELETE)
                .uri(getDirectoryServerBaseUri() + path)
                .header(HEADER_USER_ID, userId)
                .contentType(MediaType.APPLICATION_JSON)
                .body(referenceUuids)
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
     * Get the permissions for a given list of directory elements and a given user.
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

    public void createElementReferences(@NonNull UUID nodeContainerUuid, @NonNull UUID studyRootContainerUuid, String userId, List<ModificationReferenceData> referencesData) {
        // one call per referenced shared element, carrying all its references to create
        Map<UUID, List<ReferenceAttributes>> referencesAttributesByReferencedId = new LinkedHashMap<>();
        for (ModificationReferenceData ref : referencesData) {
            // local ModificationReferenceData data don't hold the node UUID so when containerId is null it means that this reference modification is at the root level
            // otherwise, the reference modification is inside a composite
            boolean insideComposite = ref.containerId() != null;
            ReferenceAttributes referenceAttributes = ReferenceAttributes.createReferenceAttributes(
                    ref.modificationUuid(),
                    insideComposite ? nodeContainerUuid : studyRootContainerUuid,
                    insideComposite ? ref.containerId() : nodeContainerUuid,
                    insideComposite ? ReferenceAttributes.ReferenceType.STUDY_NODE_NETWORK_MODIFICATION
                            : ReferenceAttributes.ReferenceType.STUDY_NODE);
            referencesAttributesByReferencedId.computeIfAbsent(ref.referencedId(), k -> new ArrayList<>()).add(referenceAttributes);
        }
        for (Map.Entry<UUID, List<ReferenceAttributes>> entry : referencesAttributesByReferencedId.entrySet()) {
            createElementReferences(entry.getKey(), entry.getValue(), userId);
        }
    }
}
