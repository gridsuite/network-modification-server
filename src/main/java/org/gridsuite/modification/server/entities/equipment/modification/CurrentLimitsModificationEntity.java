/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;
import java.util.UUID;

import jakarta.persistence.*;
import org.gridsuite.modification.model.CurrentLimitsModificationModel;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "currentLimitsModification")
public class CurrentLimitsModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "permanentLimit")
    private Double permanentLimit;

    @ElementCollection
    @CollectionTable(
            name = "currentTemporaryLimitsModification",
            joinColumns = @JoinColumn(name = "id", foreignKey = @ForeignKey(name = "temporaryLimitsModification_fk_constraint"))
    )
    private List<CurrentTemporaryLimitModificationEmbeddable> temporaryLimits;

    public CurrentLimitsModificationModel toCurrentLimitsModel() {
        return CurrentLimitsModificationModel
                .builder()
                .permanentLimit(getPermanentLimit())
                .temporaryLimits(CurrentTemporaryLimitModificationEmbeddable.fromEmbeddableCurrentTemporaryLimits(getTemporaryLimits()))
                .build();
    }

    public CurrentLimitsModificationEntity(CurrentLimitsModificationModel currentLimitsModificationInfos) {
        this(null, currentLimitsModificationInfos.getPermanentLimit(), CurrentTemporaryLimitModificationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimitsModificationInfos.getTemporaryLimits()));
    }
}
