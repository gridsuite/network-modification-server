/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.simple.SimpleModificationByFilterInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.ModificationByFilterEntity;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "simpleModification", indexes = @Index(name = "modification_by_filter_id_idx", columnList = "modification_by_filter_id"))
@Inheritance(strategy = InheritanceType.JOINED)
public class SimpleModificationEntity extends ModificationByFilterEntity {
    @Column
    @Enumerated(EnumType.STRING)
    private DataType dataType;

    public SimpleModificationEntity(SimpleModificationByFilterInfos<?> simpleModificationInfos) {
        super(simpleModificationInfos);
        this.dataType = simpleModificationInfos.getDataType();
    }

    protected void assignAttributes(SimpleModificationByFilterInfos<?> simpleModificationInfos) {
        super.assignAttributes(simpleModificationInfos);
        simpleModificationInfos.setDataType(dataType);
    }

    public <T> SimpleModificationByFilterInfos<T> toSimpleModificationInfos() {
        return null;
    }
}
