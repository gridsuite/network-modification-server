/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ReactiveCapabilityCurveCreationEmbeddable {
    @Column
    private Double qminP; //minq

    @Column
    private Double qmaxP; //maxq

    @Column
    private Double p; //
}
