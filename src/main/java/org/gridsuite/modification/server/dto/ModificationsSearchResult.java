/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.UUID;

/**
 * @author Souissi Maissa <souissi.maissa at rte-france.com>
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Modification search result")
public class ModificationsSearchResult {
    @Schema(description = "Modification id")
    private UUID modificationUuid;

    @Schema(
            description = "Message type"
    )
    private String messageType;

    @Schema(
            description = "Impacted equipment ID"
    )

    private String impactedEquipmentId;
    @Schema(
            description = "Message values"
    )
    private String messageValues;

    public static ModificationsSearchResult.ModificationsSearchResultBuilder fromModificationEntity(ModificationEntity modificationEntity) {
        return ModificationsSearchResult.builder()
                .modificationUuid(modificationEntity.getId())
                .messageType(modificationEntity.getMessageType())
                .messageValues(modificationEntity.getMessageValues());
    }
}
