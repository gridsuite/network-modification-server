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
import java.util.stream.Collectors;

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

    @Column(name = "operationalLimitGroupId")
    private String operationalLimitGroupId;

    @ElementCollection
    @CollectionTable(
            name = "currentTemporaryLimits",
            joinColumns = @JoinColumn(name = "id", foreignKey = @ForeignKey(name = "temporaryLimits_constraint_fk"))
    )
    private List<CurrentTemporaryLimitCreationEmbeddable> temporaryLimits;

    public static List<CurrentLimitsInfos> fromCurrentLimitsEntities(List<CurrentLimitsEntity> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limitEntity ->
                                CurrentLimitsInfos.builder()
                                        .operationalLimitGroupId(limitEntity.getOperationalLimitGroupId())
                                        .permanentLimit(limitEntity.getPermanentLimit())
                                        .temporaryLimits(CurrentTemporaryLimitCreationEmbeddable.fromEmbeddableCurrentTemporaryLimits(limitEntity.getTemporaryLimits()))
                                        .build()
                        )
                        .collect(Collectors.toList());
    }

    public static List<CurrentLimitsEntity> toCurrentLimitsEntities(List<CurrentLimitsInfos> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(currentLimit ->
                                new CurrentLimitsEntity(
                                        null,
                                        currentLimit.getPermanentLimit(),
                                        currentLimit.getOperationalLimitGroupId(),
                                        CurrentTemporaryLimitCreationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimit.getTemporaryLimits())
                                )
                        )
                        .collect(Collectors.toList());
    }
}
