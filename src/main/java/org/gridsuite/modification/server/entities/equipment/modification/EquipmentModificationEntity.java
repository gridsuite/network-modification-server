/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.EquipmentModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class EquipmentModificationEntity extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "equipment_modification_id")
    @OrderColumn(name = "insert_position")
    private List<FreePropertyEntity> properties;

    protected EquipmentModificationEntity(EquipmentModificationInfos equipmentModificationInfos) {
        super(equipmentModificationInfos);
        assignAttributes(equipmentModificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((EquipmentModificationInfos) modificationInfos);
    }

    private void assignAttributes(EquipmentModificationInfos equipmentModificationInfos) {
        equipmentId = equipmentModificationInfos.getEquipmentId();
        List<FreePropertyEntity> newProperties = equipmentModificationInfos.getProperties() == null ? null :
            equipmentModificationInfos.getProperties().stream()
                .map(FreePropertyEntity::new)
                .toList();
        if (this.properties != null) {
            // update using the same reference with clear/add (to avoid JPA exception)
            this.properties.clear();
            if (newProperties != null) {
                this.properties.addAll(newProperties);
            }
        } else {
            this.properties = newProperties;
        }
    }
}
