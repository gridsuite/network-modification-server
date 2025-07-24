/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentLimitsEntity;
import org.gridsuite.modification.server.entities.equipment.creation.OperationalLimitsGroupEntity;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "operational_limits_group_modification")
public class OperationalLimitsGroupModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "uuid")
    private UUID uuid;

    @Column(name = "id")
    private String id;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "current_limits_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "current_limits_id_fk"
            ))
    private CurrentLimitsModificationEntity currentLimits;

    @Column(name = "modificationType")
    @Enumerated(EnumType.STRING)
    private OperationalLimitsGroupModificationType modificationType;

    @Column(name = "temporaryLimitsModificationType")
    @Enumerated(EnumType.STRING)
    private TemporaryLimitModificationType temporaryLimitsModificationType;

    @Column(name = "side")
    private String side;

    public static List<OperationalLimitsGroupEntity> toOperationalLimitsGroupsEntities(@NonNull List<OperationalLimitsGroupInfos> limitsGroups) {
        return limitsGroups.stream()
                .filter(Objects::nonNull)
                .map(limitsGroup ->
                        new OperationalLimitsGroupEntity(
                                null,
                                limitsGroup.getId(),
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
                                        .currentLimits(limitsGroupEntity.getCurrentLimits().toCurrentLimitsInfos())
                                        .build()
                        )
                        .collect(Collectors.toList());
    }

    public OperationalLimitsGroupModificationEntity(OperationalLimitsGroupModificationInfos operationalLimitsGroupModificationInfos) {
        this(null, operationalLimitsGroupModificationInfos.getId(), new CurrentLimitsModificationEntity(operationalLimitsGroupModificationInfos.getCurrentLimits()), operationalLimitsGroupModificationInfos.getModificationType(), operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType(), operationalLimitsGroupModificationInfos.getSide());
    }

    public OperationalLimitsGroupModificationInfos toOperationalLimitsGroupModificationInfos() {
        return OperationalLimitsGroupModificationInfos
                .builder()
                .id(getId())
                .side(getSide())
                .modificationType(getModificationType())
                .temporaryLimitsModificationType(getTemporaryLimitsModificationType())
                .currentLimits(getCurrentLimits().toCurrentLimitsInfos())
                .build();
    }
}
