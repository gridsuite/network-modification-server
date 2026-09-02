/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.databind.JsonNode;

import java.util.UUID;

/**
 * One element to put inside a composite modification (create / replace).
 *
 * @param modificationUuid the modification to clone into the composite. For a shared modification the caller
 *                         (explore-server) already resolves the reference to the composite it points to, so a
 *                         plain copy is stored instead of a pointing link.
 * @param description      description to apply to the stored clone, or {@code null} to keep the source's own
 *                         description. Used to carry a selected reference's description onto the resolved
 *                         composite copy, since that description lives on the reference, not on the composite.
 *
 *                         <p>Accepts two JSON shapes for backward compatibility: a bare uuid string {@code "uuid"} (description left
 *                         null) or an object {@code {"modificationUuid": "uuid", "description": "..."}}.
 */
public record CompositeModificationContentInfos(UUID modificationUuid, String description) {

    @JsonCreator
    static CompositeModificationContentInfos fromJson(JsonNode node) {
        if (node.isTextual()) {
            return new CompositeModificationContentInfos(UUID.fromString(node.asText()), null);
        }
        JsonNode descriptionNode = node.get("description");
        String description = descriptionNode == null || descriptionNode.isNull() ? null : descriptionNode.asText();
        return new CompositeModificationContentInfos(UUID.fromString(node.get("modificationUuid").asText()), description);
    }
}
