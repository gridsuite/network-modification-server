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
import lombok.NonNull;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import jakarta.persistence.*;

import java.io.UncheckedIOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "branchStatusModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "branchStatusModification_id_fk_constraint"))
public class BranchStatusModificationEntity extends EquipmentModificationEntity {

    @Column(name = "action")
    @Enumerated(EnumType.STRING)
    private BranchStatusModificationInfos.ActionType action;

    @Column
    private String energizedVoltageLevelId;

    public BranchStatusModificationEntity(@NonNull BranchStatusModificationInfos branchStatusModificationInfos) {
        super(branchStatusModificationInfos);
        assignAttributes(branchStatusModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BranchStatusModificationInfos) modificationInfos);
    }

    private void assignAttributes(BranchStatusModificationInfos branchStatusModificationInfos) {
        action = branchStatusModificationInfos.getAction();
        energizedVoltageLevelId = branchStatusModificationInfos.getEnergizedVoltageLevelId();
    }

    @Override
    public void getAdditionalInfosForMetadata(ModificationInfos modificationInfos) {
        super.getAdditionalInfosForMetadata(modificationInfos);
        try {
            Map<String, String> messageValuesMap = new HashMap<>();
            messageValuesMap.put("action", ((BranchStatusModificationInfos) modificationInfos).getAction().name());
            messageValuesMap.put("equipmentId", ((BranchStatusModificationInfos) modificationInfos).getEquipmentId());
            messageValuesMap.put("energizedVoltageLevelId", ((BranchStatusModificationInfos) modificationInfos).getEnergizedVoltageLevelId());
            ObjectMapper objectMapper = new ObjectMapper();
            this.setMessageValues(objectMapper.writeValueAsString(messageValuesMap));
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public BranchStatusModificationInfos toModificationInfos() {
        return BranchStatusModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .equipmentId(getEquipmentId())
            .action(getAction())
            .energizedVoltageLevelId(getEnergizedVoltageLevelId())
            .build();
    }
}
