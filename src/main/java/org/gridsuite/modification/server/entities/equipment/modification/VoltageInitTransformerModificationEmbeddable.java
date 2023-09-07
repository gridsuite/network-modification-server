/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.ThreeWindingsTransformer;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class VoltageInitTransformerModificationEmbeddable {
    @Column
    private String transformerId;

    @Column
    private Integer ratioTapChangerPosition;

    @Column
    @Enumerated(EnumType.STRING)
    private ThreeWindingsTransformer.Side legSide;
}
