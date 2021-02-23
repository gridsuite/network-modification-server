/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

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
@Table(name = "stringAttribute")
public class StringAttributeEntity extends AbstractAttributeEntity {

    @Column(name = "attributeValue")
    private String attributeValue;

    public StringAttributeEntity(String attributeName, String attributeValue) {
        super(attributeName);
        this.attributeValue = attributeValue;
    }
}
