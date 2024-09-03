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
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
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

import static org.gridsuite.modification.server.dto.AttributeModification.toAttributeModification;

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
        this.maximumSectionCount = new IntegerModificationEmbedded(shuntCompensatorModificationInfos.getMaximumSectionCount());
        this.sectionCount = new IntegerModificationEmbedded(shuntCompensatorModificationInfos.getSectionCount());
        this.maxQAtNominalV = new DoubleModificationEmbedded(shuntCompensatorModificationInfos.getMaxQAtNominalV());
        this.shuntCompensatorType = new EnumModificationEmbedded<>(shuntCompensatorModificationInfos.getShuntCompensatorType());
        this.maxSusceptance = new DoubleModificationEmbedded(shuntCompensatorModificationInfos.getMaxSusceptance());
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
                .active(getActive())
                .equipmentId(getEquipmentId())
                .equipmentName(toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connectionName(toAttributeModification(getConnectionName()))
                .connectionDirection(toAttributeModification(getConnectionDirection()))
                .connectionPosition(toAttributeModification(getConnectionPosition()))
                .terminalConnected(IAttributeModificationEmbeddable.toAttributeModification(getTerminalConnected()))
                .shuntCompensatorType(toAttributeModification(getShuntCompensatorType()))
                .maxQAtNominalV(toAttributeModification(getMaxQAtNominalV()))
                .maxSusceptance(toAttributeModification(getMaxSusceptance()))
                .maximumSectionCount(toAttributeModification(getMaximumSectionCount()))
                .sectionCount(toAttributeModification(getSectionCount()))
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
