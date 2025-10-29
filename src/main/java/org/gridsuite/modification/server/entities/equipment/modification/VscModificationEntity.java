/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.HvdcLine;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VscModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.dto.AttributeModification.toAttributeModification;
/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "vscModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "vscModification_id_fk_constraint"))
public class VscModificationEntity extends BasicEquipmentModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "nominalv")),
        @AttributeOverride(name = "opType", column = @Column(name = "nominalvOp"))
    })
    private DoubleModificationEmbedded nominalV;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "r")),
        @AttributeOverride(name = "opType", column = @Column(name = "rOp"))
    })
    private DoubleModificationEmbedded r;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxp")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxpOp"))
    })
    private DoubleModificationEmbedded maxP;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "operatorActivePowerLimitSide1")),
        @AttributeOverride(name = "opType", column = @Column(name = "operatorActivePowerLimitSide1Op"))
    })
    private FloatModificationEmbedded operatorActivePowerLimitSide1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "operatorActivePowerLimitSide2")),
        @AttributeOverride(name = "opType", column = @Column(name = "operatorActivePowerLimitSide2Op"))
    })
    private FloatModificationEmbedded operatorActivePowerLimitSide2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "convertersMode")),
        @AttributeOverride(name = "opType", column = @Column(name = "convertersModeOp"))
    })
    private EnumModificationEmbedded<HvdcLine.ConvertersMode> convertersMode;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "activePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "activePowerSetpointOp"))
    })
    private DoubleModificationEmbedded activePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "angleDroopActivePowerControl")),
        @AttributeOverride(name = "opType", column = @Column(name = "angleDroopActivePowerControlOp"))
    })
    private BooleanModificationEmbedded angleDroopActivePowerControl;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "p0")),
        @AttributeOverride(name = "opType", column = @Column(name = "p0Op"))
    })
    private FloatModificationEmbedded p0;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "droop")),
        @AttributeOverride(name = "opType", column = @Column(name = "droopOp"))
    })
    private FloatModificationEmbedded droop;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "converter_station_1_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "converter_station_1_id_fk"
            ))
    private ConverterStationModificationEntity converterStation1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "converter_station_2_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "converter_station_2_id_fk"
            ))
    private ConverterStationModificationEntity converterStation2;

    public VscModificationEntity(@NonNull VscModificationInfos vscModificationInfos) {
        super(vscModificationInfos);
        assignAttributes(vscModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VscModificationInfos) modificationInfos);
    }

    private void assignAttributes(@NonNull VscModificationInfos vscModificationInfos) {
        this.nominalV = new DoubleModificationEmbedded(vscModificationInfos.getNominalV());
        this.r = new DoubleModificationEmbedded(vscModificationInfos.getR());
        this.maxP = new DoubleModificationEmbedded(vscModificationInfos.getMaxP());
        this.operatorActivePowerLimitSide1 = new FloatModificationEmbedded(vscModificationInfos.getOperatorActivePowerLimitFromSide1ToSide2());
        this.operatorActivePowerLimitSide2 = new FloatModificationEmbedded(vscModificationInfos.getOperatorActivePowerLimitFromSide2ToSide1());
        this.convertersMode = new EnumModificationEmbedded<>(vscModificationInfos.getConvertersMode());
        this.activePowerSetpoint = new DoubleModificationEmbedded(vscModificationInfos.getActivePowerSetpoint());
        this.angleDroopActivePowerControl = new BooleanModificationEmbedded(vscModificationInfos.getAngleDroopActivePowerControl());
        this.p0 = new FloatModificationEmbedded(vscModificationInfos.getP0());
        this.droop = new FloatModificationEmbedded(vscModificationInfos.getDroop());
        this.converterStation1 = new ConverterStationModificationEntity(vscModificationInfos.getConverterStation1());
        this.converterStation2 = new ConverterStationModificationEntity(vscModificationInfos.getConverterStation2());
    }

    private VscModificationInfos.VscModificationInfosBuilder<?, ?> toVscModificationInfosBuilder() {
        return VscModificationInfos.builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .nominalV(IAttributeModificationEmbeddable.toAttributeModification(getNominalV()))
                .r(IAttributeModificationEmbeddable.toAttributeModification(getR()))
                .maxP(IAttributeModificationEmbeddable.toAttributeModification(getMaxP()))
                .operatorActivePowerLimitFromSide1ToSide2(IAttributeModificationEmbeddable.toAttributeModification(getOperatorActivePowerLimitSide1()))
                .operatorActivePowerLimitFromSide2ToSide1(IAttributeModificationEmbeddable.toAttributeModification(getOperatorActivePowerLimitSide2()))
                .convertersMode(IAttributeModificationEmbeddable.toAttributeModification(getConvertersMode()))
                .activePowerSetpoint(IAttributeModificationEmbeddable.toAttributeModification(getActivePowerSetpoint()))
                .angleDroopActivePowerControl(IAttributeModificationEmbeddable.toAttributeModification(getAngleDroopActivePowerControl()))
                .p0(IAttributeModificationEmbeddable.toAttributeModification(getP0()))
                .droop(IAttributeModificationEmbeddable.toAttributeModification(getDroop()))
                .converterStation1(getConverterStation1() == null ? null : getConverterStation1().toModificationInfos())
                .converterStation2(getConverterStation2() == null ? null : getConverterStation2().toModificationInfos())
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }

    @Override
    public VscModificationInfos toModificationInfos() {
        return toVscModificationInfosBuilder().build();
    }
}
