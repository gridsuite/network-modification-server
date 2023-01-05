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

import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ReactiveCapabilityCurveModificationEmbeddable {
    @Column
    private Double qminP;

    @Column
    private Double oldQminP;

    @Column
    private Double qmaxP;

    @Column
    private Double oldQmaxP;

    @Column
    private Double p;

    @Column
    private Double oldP;
}
