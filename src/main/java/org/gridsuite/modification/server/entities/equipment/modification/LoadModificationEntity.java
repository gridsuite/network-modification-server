/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.LoadType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;

import jakarta.persistence.*;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "loadModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "loadModification_id_fk_constraint"))
public class LoadModificationEntity extends InjectionModificationEntity {
    @Column(name = "loadTypeValue")
    private LoadType loadTypeValue;

    @Column(name = "loadTypeOp")
    @Enumerated(EnumType.STRING)
    private OperationType loadTypeOp;

    @Column(name = "p0_value")
    private Double p0Value;

    @Column(name = "p0_op")
    @Enumerated(EnumType.STRING)
    private OperationType p0Op;

    @Column(name = "q0_value")
    private Double q0Value;

    @Column(name = "q0_Op")
    @Enumerated(EnumType.STRING)
    private OperationType q0Op;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "pMeasurementValue")),
        @AttributeOverride(name = "opType", column = @Column(name = "pMeasurementValueOp"))
    })
    private DoubleModificationEmbedded pMeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "pMeasurementValidity")),
        @AttributeOverride(name = "opType", column = @Column(name = "pMeasurementValidityOp"))
    })
    private BooleanModificationEmbedded pMeasurementValidity;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "qMeasurementValue")),
        @AttributeOverride(name = "opType", column = @Column(name = "qMeasurementValueOp"))
    })
    private DoubleModificationEmbedded qMeasurementValue;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "qMeasurementValidity")),
        @AttributeOverride(name = "opType", column = @Column(name = "qMeasurementValidityOp"))
    })
    private BooleanModificationEmbedded qMeasurementValidity;

    public LoadModificationEntity(@NonNull LoadModificationInfos loadModificationInfos) {
        super(loadModificationInfos);
        assignAttributes(loadModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LoadModificationInfos) modificationInfos);
    }

    private void assignAttributes(LoadModificationInfos loadModificationInfos) {
        this.loadTypeValue = loadModificationInfos.getLoadType() != null ? loadModificationInfos.getLoadType().getValue() : null;
        this.loadTypeOp = loadModificationInfos.getLoadType() != null ? loadModificationInfos.getLoadType().getOp() : null;
        this.p0Value = loadModificationInfos.getP0() != null ? loadModificationInfos.getP0().getValue() : null;
        this.p0Op = loadModificationInfos.getP0() != null ? loadModificationInfos.getP0().getOp() : null;
        this.q0Value = loadModificationInfos.getQ0() != null ? loadModificationInfos.getQ0().getValue() : null;
        this.q0Op = loadModificationInfos.getQ0() != null ? loadModificationInfos.getQ0().getOp() : null;
        this.pMeasurementValue = loadModificationInfos.getPMeasurementValue() != null ? new DoubleModificationEmbedded(loadModificationInfos.getPMeasurementValue()) : null;
        this.pMeasurementValidity = loadModificationInfos.getPMeasurementValidity() != null ? new BooleanModificationEmbedded(loadModificationInfos.getPMeasurementValidity()) : null;
        this.qMeasurementValue = loadModificationInfos.getQMeasurementValue() != null ? new DoubleModificationEmbedded(loadModificationInfos.getQMeasurementValue()) : null;
        this.qMeasurementValidity = loadModificationInfos.getQMeasurementValidity() != null ? new BooleanModificationEmbedded(loadModificationInfos.getQMeasurementValidity()) : null;
    }

    @Override
    public LoadModificationInfos toModificationInfos() {
        return toLoadModificationInfosBuilder().build();
    }

    private LoadModificationInfos.LoadModificationInfosBuilder<?, ?> toLoadModificationInfosBuilder() {
        return LoadModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connectionName(toAttributeModification(getConnectionName()))
                .connectionDirection(toAttributeModification(getConnectionDirection()))
                .connectionPosition(toAttributeModification(getConnectionPosition()))
                .terminalConnected(toAttributeModification(getTerminalConnected()))
                .loadType(AttributeModification.toAttributeModification(getLoadTypeValue(), getLoadTypeOp()))
                .p0(AttributeModification.toAttributeModification(getP0Value(), getP0Op()))
                .q0(AttributeModification.toAttributeModification(getQ0Value(), getQ0Op()))
                .pMeasurementValue(toAttributeModification(getPMeasurementValue()))
                .pMeasurementValidity(toAttributeModification(getPMeasurementValidity()))
                .qMeasurementValue(toAttributeModification(getQMeasurementValue()))
                .qMeasurementValidity(toAttributeModification(getQMeasurementValidity()))
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                    getProperties().stream()
                        .map(FreePropertyEntity::toInfos)
                        .toList());
    }
}
