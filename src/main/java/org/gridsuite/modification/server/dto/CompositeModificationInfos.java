/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Composite modification")
@JsonTypeName("COMPOSITE_MODIFICATION")
@ModificationErrorTypeName("COMPOSITE_MODIFICATION_ERROR")
public class CompositeModificationInfos extends ModificationInfos {

    @Schema(description = "modifications list")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<UUID> modificationsList;

    @Override
    public ModificationEntity toEntity() {
        return new CompositeModificationEntity(this);
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("compositeModificationType", ModificationType.COMPOSITE_MODIFICATION.name());
        return mapMessageValues;
    }
}
