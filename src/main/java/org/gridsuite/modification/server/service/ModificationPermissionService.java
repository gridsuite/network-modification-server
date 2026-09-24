/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import jakarta.annotation.Nullable;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.dto.PermissionType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.ModificationInfosUtils.contentOf;

/**
 * Fills the modification references of a payload with the permission its reader holds on the shared modification
 * they point to, so that the client doesn't have to ask the directory-server itself.
 *
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Service
public class ModificationPermissionService {
    private static final Logger LOGGER = LoggerFactory.getLogger(ModificationPermissionService.class);

    private final DirectoryService directoryService;

    public ModificationPermissionService(DirectoryService directoryService) {
        this.directoryService = directoryService;
    }

    /**
     * @return the modification it was given, its references filled with their permission
     */
    public ModificationInfos addPermissions(ModificationInfos modification, @Nullable String userId) {
        resolvePermissions(List.of(modification), userId);
        return modification;
    }

    /**
     * @return the modifications it was given, their references filled with their permission
     */
    public List<ModificationInfos> addPermissions(List<ModificationInfos> modifications, @Nullable String userId) {
        resolvePermissions(modifications, userId);
        return modifications;
    }

    /**
     * Reads the permissions of every reference of the given modifications, nested ones included, in a single call
     * to the directory-server.
     * <p>
     * A permission is left unresolved when there is no user to read it for and when the directory-server cannot
     * answer. That is the absence of an answer, global and momentary, and not an answer about the element: taking
     * it for a denial would turn every modification read-only for the time of an outage, when the write is guarded
     * on its own anyway. {@code NONE} is the opposite, an answer about the element that the user may not touch it,
     * either because no permission is held on it or because it does not exist any more.
     */
    private void resolvePermissions(Collection<ModificationInfos> modifications, @Nullable String userId) {
        if (userId == null) {
            return;
        }
        List<ModificationReferenceInfos> references = new ArrayList<>();
        modifications.forEach(modification -> collectReferences(modification, references));
        Set<UUID> referencedIds = references.stream()
                .map(ModificationReferenceInfos::getReferencedId)
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
        if (referencedIds.isEmpty()) {
            return;
        }

        Map<UUID, PermissionType> permissions;
        try {
            permissions = directoryService.getElementsPermissions(referencedIds, userId);
        } catch (RestClientException e) {
            LOGGER.warn("Could not read the permissions of the shared modifications", e);
            return;
        }
        // directory server leaves out the elements without permission, and the ones it does not know : we default it to NONE
        references.forEach(reference -> reference.setPermission(permissions.getOrDefault(reference.getReferencedId(), PermissionType.NONE)));
    }

    private static void collectReferences(ModificationInfos modification, List<ModificationReferenceInfos> references) {
        if (modification instanceof ModificationReferenceInfos reference) {
            references.add(reference);
        }
        contentOf(modification).forEach(content -> collectReferences(content, references));
    }
}
