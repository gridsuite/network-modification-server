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
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.dto.AttributeModification.toAttributeModification;

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
        @AttributeOverride(name = "value", column = @Column(name = "nominalV")),
        @AttributeOverride(name = "opType", column = @Column(name = "nominalV_Op"))
    })
    private DoubleModificationEmbedded nominalV;

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
        this.nominalV = voltageLevelModificationInfos.getNominalV() != null ? new DoubleModificationEmbedded(voltageLevelModificationInfos.getNominalV()) : null;
        this.lowVoltageLimit = voltageLevelModificationInfos.getLowVoltageLimit() != null ? new DoubleModificationEmbedded(voltageLevelModificationInfos.getLowVoltageLimit()) : null;
        this.highVoltageLimit = voltageLevelModificationInfos.getHighVoltageLimit() != null ? new DoubleModificationEmbedded(voltageLevelModificationInfos.getHighVoltageLimit()) : null;
        this.ipMin = voltageLevelModificationInfos.getIpMin() != null ? new DoubleModificationEmbedded(voltageLevelModificationInfos.getIpMin()) : null;
        this.ipMax = voltageLevelModificationInfos.getIpMax() != null ? new DoubleModificationEmbedded(voltageLevelModificationInfos.getIpMax()) : null;
    }

    @Override
    public VoltageLevelModificationInfos toModificationInfos() {
        return toVoltageLevelModificationInfosBuilder().build();
    }

    private VoltageLevelModificationInfos.VoltageLevelModificationInfosBuilder<?, ?> toVoltageLevelModificationInfosBuilder() {
        return VoltageLevelModificationInfos.builder()
                .uuid(getId())
                .equipmentId(getEquipmentId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentName(toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .nominalV(IAttributeModificationEmbeddable.toAttributeModification(getNominalV()))
                .lowVoltageLimit(IAttributeModificationEmbeddable.toAttributeModification(getLowVoltageLimit()))
                .highVoltageLimit(IAttributeModificationEmbeddable.toAttributeModification(getHighVoltageLimit()))
                .ipMin(IAttributeModificationEmbeddable.toAttributeModification(this.getIpMin()))
                .ipMax(IAttributeModificationEmbeddable.toAttributeModification(this.getIpMax()))
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());

    }
}
