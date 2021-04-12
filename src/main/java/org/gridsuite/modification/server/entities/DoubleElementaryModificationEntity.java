/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import java.util.Set;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "doubleelementaryModification")
public class DoubleElementaryModificationEntity extends AbstractElementaryModificationEntity {

    @Column(name = "attributeValue")
    @Getter(AccessLevel.NONE)
    private Double attributeValue;

    @Override
    public Double getAttributeValue() {
        return attributeValue;
    }

    public DoubleElementaryModificationEntity(String equipmentId, Set<String> substationId, String attributeName, Double attributeValue) {
        super(equipmentId, substationId, attributeName);
        this.attributeValue = attributeValue;
    }
}
