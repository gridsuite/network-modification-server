/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "LoadScalingVariation")
public class LoadScalingVariationEntity {

    @Id
    @Column(name = "id", nullable = false)
    private String id;

    @Column(name = "filterId")
    private String filterId;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "activeVariationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode activeVariationMode;

    @Column(name = "reactiveVariationMode")
    @Enumerated(EnumType.STRING)
    private ReactiveVariationMode reactiveVariationMode;
}
