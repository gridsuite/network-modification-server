/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.simple;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.PropertySimpleModificationEntity;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
public class PropertySimpleModificationInfos extends StringSimpleModificationInfos {
    @Schema(description = "property name")
    private String propertyName;

    @Override
    public PropertySimpleModificationEntity toEntity() {
        PropertySimpleModificationEntity propertySimpleModificationEntity = new PropertySimpleModificationEntity(this);
        propertySimpleModificationEntity.setPropertyName(propertyName);
        return propertySimpleModificationEntity;
    }
}
