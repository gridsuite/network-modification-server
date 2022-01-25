/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import javax.persistence.Embeddable;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class BusbarSectionCreationEmbeddable {
    private String id;

    private String name;

    private int vertPos;

    private int horizPos;
}
