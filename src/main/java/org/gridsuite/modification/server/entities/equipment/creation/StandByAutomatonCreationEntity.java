/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.UUID;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Entity
@Table(name = "standByAutomatonCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "standByAutomatonCreation_id_fk_constraint"))
public class StandByAutomatonCreationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private boolean standby;

    @Column
    private double b0;

    @Column
    private double lowVoltageSetpoint;

    @Column
    private double highVoltageSetpoint;

    @Column
    private double lowVoltageThreshold;

    @Column
    private double highVoltageThreshold;
}
