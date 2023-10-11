/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;

import java.io.UncheckedIOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class EquipmentModificationEntity extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    protected EquipmentModificationEntity(EquipmentModificationInfos equipmentModificationInfos) {
        super(equipmentModificationInfos);
        assignAttributes(equipmentModificationInfos);
    }

    @Override
    public void getModificationMetadata(ModificationInfos modificationInfos) {
        super.getModificationMetadata(modificationInfos);
        try {
            Map<String, String> messageValuesMap = new HashMap<>();
            messageValuesMap.put("equipmentId", ((EquipmentModificationInfos) modificationInfos).getEquipmentId());
            ObjectMapper objectMapper = new ObjectMapper();
            this.setMessageValues(objectMapper.writeValueAsString(messageValuesMap));
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((EquipmentModificationInfos) modificationInfos);
    }

    private void assignAttributes(EquipmentModificationInfos equipmentModificationInfos) {
        equipmentId = equipmentModificationInfos.getEquipmentId();
    }
}
