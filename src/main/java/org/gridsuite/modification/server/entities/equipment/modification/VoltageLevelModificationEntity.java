/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import static org.gridsuite.modification.server.dto.AttributeModification.toAttributeModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "voltageLevelModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "voltageLevelModification_id_fk_constraint"))
public class VoltageLevelModificationEntity extends BasicEquipmentModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "nominalVoltage")),
            @AttributeOverride(name = "opType", column = @Column(name = "nominalVoltageOp"))
    })
    private DoubleModificationEmbedded nominalVoltage;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "lowVoltageLimit")),
            @AttributeOverride(name = "opType", column = @Column(name = "lowVoltageLimitOp"))
    })
    private DoubleModificationEmbedded lowVoltageLimit;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "highVoltageLimit")),
            @AttributeOverride(name = "opType", column = @Column(name = "highVoltageLimitOp"))
    })
    private DoubleModificationEmbedded highVoltageLimit;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "ipMin")),
            @AttributeOverride(name = "opType", column = @Column(name = "ipMinOp"))
    })
    private DoubleModificationEmbedded ipMin;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "ipMax")),
            @AttributeOverride(name = "opType", column = @Column(name = "ipMaxOp"))
    })
    private DoubleModificationEmbedded ipMax;

    public VoltageLevelModificationEntity(VoltageLevelModificationInfos voltageLevelModificationInfos) {
        super(voltageLevelModificationInfos);
        assignAttributes(voltageLevelModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VoltageLevelModificationInfos) modificationInfos);
    }

    private void assignAttributes(VoltageLevelModificationInfos voltageLevelModificationInfos) {
        this.nominalVoltage = new DoubleModificationEmbedded(voltageLevelModificationInfos.getNominalVoltage());
        this.lowVoltageLimit = new DoubleModificationEmbedded(voltageLevelModificationInfos.getLowVoltageLimit());
        this.highVoltageLimit = new DoubleModificationEmbedded(voltageLevelModificationInfos.getHighVoltageLimit());
        this.ipMin = new DoubleModificationEmbedded(voltageLevelModificationInfos.getIpMin());
        this.ipMax = new DoubleModificationEmbedded(voltageLevelModificationInfos.getIpMax());
    }

    @Override
    public VoltageLevelModificationInfos toModificationInfos() {
        return toVoltageLevelModificationInfosBuilder().build();
    }

    private VoltageLevelModificationInfos.VoltageLevelModificationInfosBuilder<?, ?> toVoltageLevelModificationInfosBuilder() {
        return VoltageLevelModificationInfos.builder()
                .uuid(getId())
                .groupUuid(getGroup().getId())
                .equipmentId(getEquipmentId())
                .date(getDate())
                .equipmentName(toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .nominalVoltage(toAttributeModification(getNominalVoltage()))
                .lowVoltageLimit(toAttributeModification(getLowVoltageLimit()))
                .highVoltageLimit(toAttributeModification(getHighVoltageLimit()))
                .ipMin(toAttributeModification(this.getIpMin()))
                .ipMax(toAttributeModification(this.getIpMax()));

    }
}
