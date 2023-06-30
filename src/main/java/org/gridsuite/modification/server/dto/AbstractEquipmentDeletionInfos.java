/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.deletion.AbstractEquipmentDeletionEntity;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */

@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = HvdcLccDeletionInfos.class, name = "HVDC_LINE_WITH_LCC")
})

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public abstract class AbstractEquipmentDeletionInfos {
    public AbstractEquipmentDeletionEntity toEntity() {
        return null;
    }
}
