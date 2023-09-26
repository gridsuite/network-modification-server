/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class VoltageInitGeneratorModificationEmbeddable {
    @Column
    private String generatorId;

    @Column
    private Double voltageSetpoint;

    @Column
    private Double reactivePowerSetpoint;
}
