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
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Table;

import static org.gridsuite.modification.server.dto.AttributeModification.toAttributeModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "shuntCompensatorModification")
public class ShuntCompensatorModificationEntity extends BasicEquipmentModificationEntity {
    @Column(name = "voltageLevelId")
    private String voltageLevelId;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "susceptancePerSection")),
            @AttributeOverride(name = "opType", column = @Column(name = "susceptancePerSectionOp"))
    })
    private DoubleModificationEmbedded susceptancePerSection;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "qAtNominalV")),
            @AttributeOverride(name = "opType", column = @Column(name = "qAtNominalVOp"))
    })
    private DoubleModificationEmbedded qAtNominalV;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "shuntCompensatorType")),
            @AttributeOverride(name = "opType", column = @Column(name = "shuntCompensatorTypeOp"))
    })
    private EnumModificationEmbedded<ShuntCompensatorType> shuntCompensatorType;

    public ShuntCompensatorModificationEntity(ShuntCompensatorModificationInfos shuntCompensatorModificationInfos) {
        super(shuntCompensatorModificationInfos);
        assignAttributes(shuntCompensatorModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ShuntCompensatorModificationInfos) modificationInfos);
    }

    private void assignAttributes(ShuntCompensatorModificationInfos shuntCompensatorModificationInfos) {
        this.qAtNominalV = new DoubleModificationEmbedded(shuntCompensatorModificationInfos.getQAtNominalV());
        this.shuntCompensatorType = new EnumModificationEmbedded<>(shuntCompensatorModificationInfos.getShuntCompensatorType());
        this.susceptancePerSection = new DoubleModificationEmbedded(shuntCompensatorModificationInfos.getSusceptancePerSection());
    }

    @Override
    public ShuntCompensatorModificationInfos toModificationInfos() {
        return toShuntCompensatorModificationInfosBuilder().build();
    }

    private ShuntCompensatorModificationInfos.ShuntCompensatorModificationInfosBuilder<?, ?> toShuntCompensatorModificationInfosBuilder() {
        return ShuntCompensatorModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .voltageLevelId(getVoltageLevelId())
                .equipmentId(getEquipmentId())
                .equipmentName(toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .shuntCompensatorType(toAttributeModification(getShuntCompensatorType()))
                .qAtNominalV(toAttributeModification(getQAtNominalV()))
                .susceptancePerSection(toAttributeModification(getSusceptancePerSection()));
    }
}
