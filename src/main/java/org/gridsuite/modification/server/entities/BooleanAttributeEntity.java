/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "booleanattribute")
public class BooleanAttributeEntity extends AbstractAttributeEntity {

    @Column(name = "attributeValue")
    @Getter(AccessLevel.NONE)
    private boolean attributeValue;

    @Override
    public Boolean getAttributeValue() {
        return attributeValue;
    }

    public BooleanAttributeEntity(String attributeName, boolean attributeValue) {
        super(attributeName);
        this.attributeValue = attributeValue;
    }
}
