/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.simple;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.SimpleModificationEntity;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
public class PropertyModificationByFilterInfos extends StringModificationByFilterInfos {
    @Schema(description = "Property name")
    @Getter
    private String propertyName;

    @Override
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public DataType getDataType() {
        return DataType.PROPERTY;
    }

    @Override
    public SimpleModificationEntity toEntity() {
        SimpleModificationEntity simpleModificationEntity = super.toEntity();
        simpleModificationEntity.setPropertyName(propertyName);
        return simpleModificationEntity;
    }
}
