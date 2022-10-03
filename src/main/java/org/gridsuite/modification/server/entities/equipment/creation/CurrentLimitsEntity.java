/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.CurrentLimitsInfos;

import java.util.UUID;

import javax.persistence.Column;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "currentLimits")
public class CurrentLimitsEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "permanentLimit")
    private Double permanentLimit;

    public CurrentLimitsInfos toCurrentLimitsInfos() {
        return toCurrentLimitsInfosBuilder().build();
    }

    private CurrentLimitsInfos.CurrentLimitsInfosBuilder<?, ?> toCurrentLimitsInfosBuilder() {
        return CurrentLimitsInfos
            .builder()
            .permanentLimit(getPermanentLimit());
    }
}
