/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.byfilter.simple.DoubleSimpleModificationByFilterInfos;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Data
@Entity
@Table(name = "simpleModificationDouble")
public class DoubleSimpleModificationEntity extends SimpleModificationEntity {
    @Column(name = "value_") // "value" is not supported in UT with H2
    Double value;

    public DoubleSimpleModificationEntity(DoubleSimpleModificationByFilterInfos doubleSimpleModificationByFilterInfos) {
        super(doubleSimpleModificationByFilterInfos);
        this.value = doubleSimpleModificationByFilterInfos.getValue();
    }

    @Override
    public DoubleSimpleModificationByFilterInfos toSimpleModificationInfos() {
        DoubleSimpleModificationByFilterInfos doubleSimpleModificationByFilterInfos = DoubleSimpleModificationByFilterInfos.builder()
                .value(value)
                .build();
        assignAttributes(doubleSimpleModificationByFilterInfos);
        return doubleSimpleModificationByFilterInfos;
    }
}
