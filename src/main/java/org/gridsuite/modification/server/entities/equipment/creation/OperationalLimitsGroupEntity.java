/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.*;
import org.gridsuite.modification.dto.LimitsPropertyInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LimitsPropertyEntity;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Mathieu Deharbe <mathieu.deharbe_externe at rte-france.com>
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "operational_limits_group")
public class OperationalLimitsGroupEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "uuid")
    private UUID uuid;

    @Column(name = "id")
    private String id;

    @Column(name = "applicability")
    @Enumerated(EnumType.STRING)
    private OperationalLimitsGroupInfos.Applicability applicability;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "operational_limit_group_id", foreignKey = @ForeignKey(name = "operational_limit_group_id_fk"))
    private List<LimitsPropertyEntity> limitsProperties;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "current_limits_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "current_limits_id_fk"
            ))
    private CurrentLimitsEntity currentLimits;

    private static List<LimitsPropertyEntity> toLimitPropertyEntities(List<LimitsPropertyInfos> properties) {
        List<LimitsPropertyEntity> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(properties)) {
            result = properties.stream().map(LimitsPropertyEntity::fromLimitsPropertyInfos).toList();
        }
        return result;
    }

    public static List<OperationalLimitsGroupEntity> toOperationalLimitsGroupsEntities(@NonNull List<OperationalLimitsGroupInfos> limitsGroups) {
        return limitsGroups.stream()
                .filter(Objects::nonNull)
                .map(limitsGroup ->
                        new OperationalLimitsGroupEntity(
                                null,
                                limitsGroup.getId(),
                                limitsGroup.getApplicability(),
                                toLimitPropertyEntities(limitsGroup.getLimitsProperties()),
                                new CurrentLimitsEntity(limitsGroup.getCurrentLimits())
                        )
                )
                .toList();
    }

    public static List<OperationalLimitsGroupInfos> fromOperationalLimitsGroupsEntities(List<OperationalLimitsGroupEntity> limitsGroupsEntities) {
        return CollectionUtils.isEmpty(limitsGroupsEntities) ? null :
                limitsGroupsEntities.stream()
                        .map(limitsGroupEntity ->
                                OperationalLimitsGroupInfos.builder()
                                        .id(limitsGroupEntity.getId())
                                        .applicability(limitsGroupEntity.getApplicability())
                                        .currentLimits(limitsGroupEntity.getCurrentLimits().toCurrentLimitsInfos())
                                        .limitsProperties(limitsGroupEntity.getLimitsProperties()
                                            .stream().map(LimitsPropertyEntity::toLimitsPropertyInfos).toList())
                                        .build()
                        )
                        .collect(Collectors.toList());
    }
}
