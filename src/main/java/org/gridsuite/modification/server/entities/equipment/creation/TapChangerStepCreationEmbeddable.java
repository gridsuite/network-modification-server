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
import org.gridsuite.modification.server.TapChangerType;

import javax.persistence.*;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class TapChangerStepCreationEmbeddable {

    @Column(name = "tapchangertype")
    @Enumerated(EnumType.STRING)
    private TapChangerType tapChangerType;

    @Column(name = "index")
    private int index;

    @Column(name = "rho")
    private double rho;

    @Column(name = "r")
    private double r;

    @Column(name = "x")
    private double x;

    @Column(name = "g")
    private double g;

    @Column(name = "b")
    private double b;

    @Column(name = "alpha")
    private Double alpha;
}
