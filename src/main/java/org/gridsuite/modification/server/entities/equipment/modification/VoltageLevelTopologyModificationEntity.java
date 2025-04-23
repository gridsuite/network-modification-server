/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelTopologyModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author REHILI Ghazwa <ghazwarhili@gmail.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "voltageLevelTopologyModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "voltageLevelTopologyModification_id_fk_constraint"))
public class VoltageLevelTopologyModificationEntity extends EquipmentModificationEntity {

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "voltage_level_topology_modification_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "voltageLevel_booleanEquipmentAttributeModification_fk"
            ))
    @OrderBy
    private List<BooleanEquipmentAttributeModificationEntity> equipmentAttributeModification;

    public VoltageLevelTopologyModificationEntity(VoltageLevelTopologyModificationInfos voltageLevelTopologyModificationInfos) {
        super(voltageLevelTopologyModificationInfos);
        assignAttributes(voltageLevelTopologyModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VoltageLevelTopologyModificationInfos) modificationInfos);
    }

    private void assignAttributes(VoltageLevelTopologyModificationInfos voltageLevelTopologyModificationInfos) {
        // Initialize the collection if it's null
        if (this.equipmentAttributeModification == null) {
            this.equipmentAttributeModification = new ArrayList<>();
        } else {
            this.equipmentAttributeModification.clear();
        }

        // Add new entities to the existing collection
        if (voltageLevelTopologyModificationInfos.getEquipmentAttributeModification() != null) {
            List<BooleanEquipmentAttributeModificationEntity> newEntities =
                    voltageLevelTopologyModificationInfos.getEquipmentAttributeModification().stream()
                            .map(BooleanEquipmentAttributeModificationEntity::new)
                            .collect(Collectors.toList());

            this.equipmentAttributeModification.addAll(newEntities);
        }
    }

    @Override
    public VoltageLevelTopologyModificationInfos toModificationInfos() {
        return toVoltageLevelTopologyModificationInfosBuilder().build();
    }

    private VoltageLevelTopologyModificationInfos.VoltageLevelTopologyModificationInfosBuilder<?, ?> toVoltageLevelTopologyModificationInfosBuilder() {
        List<BooleanEquipmentAttributeModificationEntity> attributeModificationEntities = getEquipmentAttributeModification();
        return VoltageLevelTopologyModificationInfos.builder()
                .uuid(getId())
                .equipmentId(getEquipmentId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentAttributeModification(Optional.ofNullable(attributeModificationEntities)
                        .map(list -> list.stream()
                                .map(EquipmentAttributeModificationEntity::toModificationInfos)
                                .toList())
                        .orElse(null));
    }
}
