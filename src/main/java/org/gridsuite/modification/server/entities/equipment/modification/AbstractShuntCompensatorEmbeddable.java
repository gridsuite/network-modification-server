/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.MappedSuperclass;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
@MappedSuperclass
public abstract class AbstractShuntCompensatorEmbeddable {
    @Column(name = "shunt_compensator_id")
    private String id;

    @Column(name = "shunt_compensator_name")
    private String name;

    @Column(name = "maxqat_nominalv")
    private Double maxQAtNominalV;

    @Column(name = "connected_to_hvdc")
    private Boolean connectedToHvdc;
}
