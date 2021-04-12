/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.elementary;

import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import java.util.Set;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "doubleelementaryModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(
        name = "double_modification_uuid_fk_constraint"
))
public class DoubleElementaryModificationEntity extends ElementaryModificationEntity<Double> {
    public DoubleElementaryModificationEntity(String equipmentId, Set<String> substationId, String attributeName, Double attributeValue) {
        super(equipmentId, substationId, attributeName, attributeValue);
    }
}
