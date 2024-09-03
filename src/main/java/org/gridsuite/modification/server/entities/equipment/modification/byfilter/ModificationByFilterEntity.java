/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;

import java.util.UUID;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@MappedSuperclass
public class ModificationByFilterEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private String editedField;

    protected ModificationByFilterEntity(AbstractModificationByFilterInfos modificationByFilterInfos) {
        this.id = null;
        this.editedField = modificationByFilterInfos.getEditedField();
    }

    protected void assignAttributes(AbstractModificationByFilterInfos modificationByFilterInfos) {
        modificationByFilterInfos.setId(id);
        modificationByFilterInfos.setEditedField(editedField);
    }
}