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
import org.gridsuite.modification.dto.CurrentLimitsInfos;

import java.util.List;
import java.util.UUID;

import jakarta.persistence.*;

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

    @ElementCollection
    @CollectionTable(
            name = "currentTemporaryLimits",
            joinColumns = @JoinColumn(name = "id", foreignKey = @ForeignKey(name = "temporaryLimits_fk_constraint"))
    )
    private List<CurrentTemporaryLimitCreationEmbeddable> temporaryLimits;

    public CurrentLimitsInfos toCurrentLimitsInfos() {
        return CurrentLimitsInfos
                .builder()
                .permanentLimit(getPermanentLimit())
                .temporaryLimits(CurrentTemporaryLimitCreationEmbeddable.fromEmbeddableCurrentTemporaryLimits(getTemporaryLimits()))
                .build();
    }

    public CurrentLimitsEntity(CurrentLimitsInfos currentLimitsInfos) {
        this(null, currentLimitsInfos.getPermanentLimit(), CurrentTemporaryLimitCreationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimitsInfos.getTemporaryLimits()));
    }
}
