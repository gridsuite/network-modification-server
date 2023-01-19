/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.MappedSuperclass;
import javax.validation.constraints.NotNull;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@MappedSuperclass
public class ScalingEntity extends ModificationEntity {
    @Column(name = "VariationType")
    @Enumerated(EnumType.STRING)
    private VariationType variationType;

    public ScalingEntity(@NotNull ModificationType modificationType) {
        super(modificationType);
    }
}
