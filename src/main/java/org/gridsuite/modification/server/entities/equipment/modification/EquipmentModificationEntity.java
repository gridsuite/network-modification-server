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
import org.gridsuite.modification.dto.ModificationDto;
import org.gridsuite.modification.model.EquipmentModificationModel;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.ArrayList;
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

    @OneToMany(mappedBy = "modification", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn(name = "insert_position")
    private List<FreePropertyEntity> properties = new ArrayList<>();

    protected EquipmentModificationEntity(ModificationDto equipmentModificationInfos) {
        super(equipmentModificationInfos);
        assignAttributes((EquipmentModificationModel) equipmentModificationInfos);
    }

    protected EquipmentModificationEntity(EquipmentModificationModel equipmentModificationModel) {
        super();
        assignAttributes(equipmentModificationModel);
    }

    @Override
    public void update(ModificationDto modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((EquipmentModificationModel) modificationInfos);
    }

    private void assignAttributes(EquipmentModificationModel equipmentModificationInfos) {
        equipmentId = equipmentModificationInfos.getEquipmentId();
        List<FreePropertyEntity> newProperties = equipmentModificationInfos.getProperties() == null ? new ArrayList<>() :
            equipmentModificationInfos.getProperties().stream()
                    .map(info -> {
                        FreePropertyEntity entity = new FreePropertyEntity(info);
                        entity.setModification(this);
                        return entity;
                    }).toList();
        // update using the same reference with clear/add (to avoid JPA exception)
        this.properties.clear();
        this.properties.addAll(newProperties);
    }
}
