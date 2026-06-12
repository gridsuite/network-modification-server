/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.*;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;

import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "modification_reference")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "modification_reference_id_fk_constraint"))
public class ModificationReferenceEntity extends ModificationEntity {

    @Column(name = "referenceId")
    UUID referenceId;

    @Column(name = "referenceType")
    String referenceType;

    // Transient just for optimization purpose
    @Transient
    private ModificationInfos referenceInfos;

    public ModificationReferenceEntity(@NonNull ModificationReferenceInfos modificationReferenceInfos) {
        super(modificationReferenceInfos);
        assignAttributes(modificationReferenceInfos);
    }

    @Override
    public ModificationReferenceInfos toModificationInfos() {
        return ModificationReferenceInfos.builder()
            .uuid(getId())
            .messageType(getMessageType())
            .messageValues(getMessageValues())
            .activated(getActivated())
            .description(getDescription())
            .date(getDate())
            .stashed(getStashed())
            .referenceId(getReferenceId())
            .referenceType(ModificationReferenceInfos.Type.valueOf(getReferenceType()))
            .referenceInfos(getReferenceInfos())
            .build();
    }

    @Override
    // Perhaps we should not allow updating reference info (recreating is better)
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ModificationReferenceInfos) modificationInfos);
    }

    @SneakyThrows
    private void assignAttributes(ModificationReferenceInfos modificationReferenceInfos) {
        modificationReferenceInfos.check();

        this.referenceType = modificationReferenceInfos.getReferenceType().name();
        this.referenceId = modificationReferenceInfos.getReferenceId();

        // Appears as the referenced modification
        this.setMessageType(modificationReferenceInfos.getMessageType());
        this.setMessageValues(modificationReferenceInfos.getMessageValues());

        // Transient just for optimization purpose
        // No need to load the referenced modification
        this.referenceInfos = modificationReferenceInfos.getReferenceInfos();
    }
}
