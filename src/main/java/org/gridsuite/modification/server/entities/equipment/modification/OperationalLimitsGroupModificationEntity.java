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
import org.gridsuite.modification.dto.OperationalLimitsGroupModificationInfos;
import org.gridsuite.modification.dto.OperationalLimitsGroupModificationType;
import org.gridsuite.modification.dto.TemporaryLimitModificationType;
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

    public static List<OperationalLimitsGroupModificationEntity> toOperationalLimitsGroupsEntities(@NonNull List<OperationalLimitsGroupModificationInfos> limitsGroups) {
        return limitsGroups.stream()
                .filter(Objects::nonNull)
                .map(limitsGroup ->
                        new OperationalLimitsGroupModificationEntity(
                                null,
                                limitsGroup.getId(),
                                new CurrentLimitsModificationEntity(limitsGroup.getCurrentLimits()),
                                limitsGroup.getModificationType(),
                                limitsGroup.getTemporaryLimitsModificationType(),
                                limitsGroup.getSide()
                        )
                )
                .toList();
    }

    public static List<OperationalLimitsGroupModificationInfos> fromOperationalLimitsGroupsEntities(List<OperationalLimitsGroupModificationEntity> limitsGroupsEntities) {
        return CollectionUtils.isEmpty(limitsGroupsEntities) ? null :
                limitsGroupsEntities.stream()
                        .map(limitsGroupEntity ->
                                OperationalLimitsGroupModificationInfos.builder()
                                        .id(limitsGroupEntity.getId())
                                        .currentLimits(limitsGroupEntity.getCurrentLimits().toCurrentLimitsInfos())
                                        .modificationType(limitsGroupEntity.getModificationType())
                                        .temporaryLimitsModificationType(limitsGroupEntity.getTemporaryLimitsModificationType())
                                        .side(limitsGroupEntity.getSide())
                                        .build()
                        )
                        .collect(Collectors.toList());
    }

    public OperationalLimitsGroupModificationEntity(OperationalLimitsGroupModificationInfos operationalLimitsGroupModificationInfos) {
        this(null, operationalLimitsGroupModificationInfos.getId(), new CurrentLimitsModificationEntity(operationalLimitsGroupModificationInfos.getCurrentLimits()), operationalLimitsGroupModificationInfos.getModificationType(), operationalLimitsGroupModificationInfos.getTemporaryLimitsModificationType(), operationalLimitsGroupModificationInfos.getSide());
    }
}
