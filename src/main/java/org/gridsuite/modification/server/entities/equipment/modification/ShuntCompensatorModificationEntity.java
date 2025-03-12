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
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerModificationEmbedded;

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.dto.AttributeModification.toAttributeModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "shuntCompensatorModification")
public class ShuntCompensatorModificationEntity extends InjectionModificationEntity {

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maximumSectionCount")),
        @AttributeOverride(name = "opType", column = @Column(name = "maximumSectionCountOp"))
    })
    private IntegerModificationEmbedded maximumSectionCount;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "sectionCount")),
        @AttributeOverride(name = "opType", column = @Column(name = "sectionCountOp"))
    })
    private IntegerModificationEmbedded sectionCount;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxSusceptance")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxSusceptanceOp"))
    })
    private DoubleModificationEmbedded maxSusceptance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxQAtNominalV")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxQAtNominalVOp"))
    })
    private DoubleModificationEmbedded maxQAtNominalV;

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
        this.maximumSectionCount = shuntCompensatorModificationInfos.getMaximumSectionCount() != null ? new IntegerModificationEmbedded(shuntCompensatorModificationInfos.getMaximumSectionCount()) : null;
        this.sectionCount = shuntCompensatorModificationInfos.getSectionCount() != null ? new IntegerModificationEmbedded(shuntCompensatorModificationInfos.getSectionCount()) : null;
        this.maxQAtNominalV = shuntCompensatorModificationInfos.getMaxQAtNominalV() != null ? new DoubleModificationEmbedded(shuntCompensatorModificationInfos.getMaxQAtNominalV()) : null;
        this.shuntCompensatorType = shuntCompensatorModificationInfos.getShuntCompensatorType() != null ? new EnumModificationEmbedded<>(shuntCompensatorModificationInfos.getShuntCompensatorType()) : null;
        this.maxSusceptance = shuntCompensatorModificationInfos.getMaxSusceptance() != null ? new DoubleModificationEmbedded(shuntCompensatorModificationInfos.getMaxSusceptance()) : null;
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
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connectionName(IAttributeModificationEmbeddable.toAttributeModification(getConnectionName()))
                .connectionDirection(IAttributeModificationEmbeddable.toAttributeModification(getConnectionDirection()))
                .connectionPosition(IAttributeModificationEmbeddable.toAttributeModification(getConnectionPosition()))
                .terminalConnected(IAttributeModificationEmbeddable.toAttributeModification(getTerminalConnected()))
                .shuntCompensatorType(IAttributeModificationEmbeddable.toAttributeModification(getShuntCompensatorType()))
                .maxQAtNominalV(IAttributeModificationEmbeddable.toAttributeModification(getMaxQAtNominalV()))
                .maxSusceptance(IAttributeModificationEmbeddable.toAttributeModification(getMaxSusceptance()))
                .maximumSectionCount(IAttributeModificationEmbeddable.toAttributeModification(getMaximumSectionCount()))
                .sectionCount(IAttributeModificationEmbeddable.toAttributeModification(getSectionCount()))
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
