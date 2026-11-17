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
import org.gridsuite.modification.server.dto.PermissionType;
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
     * Tells, on the references of the given modification, whether they are editable by the given user.
     */
    public void addPermissions(ModificationInfos modification, String userId) {
        resolvePermissions(List.of(modification), userId);
    }

    /**
     * Tells, on the references of the given modifications, whether they are editable by the given user.
     */
    public void addPermissions(List<ModificationInfos> modifications, String userId) {
        resolvePermissions(modifications, userId);
    }

    /**
     * Reads the permissions of every reference of the given modifications, nested ones included, in a single call
     * to the directory-server.
     * <p>
     * Nothing is told when the directory-server cannot answer.
     */
    private void resolvePermissions(Collection<ModificationInfos> modifications, String userId) {
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
        // directory server leaves out the elements without permission, and the ones it does not know : they are not editable
        references.forEach(reference -> reference.setEditable(isEditable(permissions.get(reference.getReferencedId()))));
    }

    private static boolean isEditable(@Nullable PermissionType permission) {
        return permission != null && permission.grants(PermissionType.WRITE);
    }

    private static void collectReferences(ModificationInfos modification, List<ModificationReferenceInfos> references) {
        if (modification instanceof ModificationReferenceInfos reference) {
            references.add(reference);
        }
        contentOf(modification).forEach(content -> collectReferences(content, references));
    }
}
