/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.StaticVarCompensator;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.StaticVarCompensatorModificationInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "staticVarCompensatorModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "staticVarCompensatorModification_id_fk_constraint"))
public class StaticVarCompensatorModificationEntity extends InjectionModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxSusceptance")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxSusceptanceOp"))
    })
    private DoubleModificationEmbedded maxSusceptance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minSusceptance")),
        @AttributeOverride(name = "opType", column = @Column(name = "minSusceptanceOp"))
    })
    private DoubleModificationEmbedded minSusceptance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxQAtNominalV")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxQAtNominalVOp"))
    })
    private DoubleModificationEmbedded maxQAtNominalV;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minQAtNominalV")),
        @AttributeOverride(name = "opType", column = @Column(name = "minQAtNominalVOp"))
    })
    private DoubleModificationEmbedded minQAtNominalV;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulationMode")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulationModeOp"))
    })
    private EnumModificationEmbedded<StaticVarCompensator.RegulationMode> regulationMode;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulating")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingOp"))
    })
    private BooleanModificationEmbedded regulating;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageSetpointOp"))
    })
    private DoubleModificationEmbedded voltageSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "reactivePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerSetpointOp"))
    })
    private DoubleModificationEmbedded reactivePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationType")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationTypeOp"))
    })
    private EnumModificationEmbedded<VoltageRegulationType> voltageRegulationType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalIdOp"))
    })
    private StringModificationEmbedded regulatingTerminalId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalType")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalTypeOp"))
    })
    private StringModificationEmbedded regulatingTerminalType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalVlId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalVlIdOp"))
    })
    private StringModificationEmbedded regulatingTerminalVlId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "standbyAutomatonOn")),
        @AttributeOverride(name = "opType", column = @Column(name = "standbyAutomatonOnOp"))
    })
    private BooleanModificationEmbedded standbyAutomatonOn;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "standby")),
        @AttributeOverride(name = "opType", column = @Column(name = "standbyOp"))
    })
    private BooleanModificationEmbedded standby;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "b0")),
        @AttributeOverride(name = "opType", column = @Column(name = "b0Op"))
    })
    private DoubleModificationEmbedded b0;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "q0")),
        @AttributeOverride(name = "opType", column = @Column(name = "q0Op"))
    })
    private DoubleModificationEmbedded q0;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "lowVoltageSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "lowVoltageSetpointOp"))
    })
    private DoubleModificationEmbedded lowVoltageSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "highVoltageSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "highVoltageSetpointOp"))
    })
    private DoubleModificationEmbedded highVoltageSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "lowVoltageThreshold")),
        @AttributeOverride(name = "opType", column = @Column(name = "lowVoltageThresholdOp"))
    })
    private DoubleModificationEmbedded lowVoltageThreshold;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "highVoltageThreshold")),
        @AttributeOverride(name = "opType", column = @Column(name = "highVoltageThresholdOp"))
    })
    private DoubleModificationEmbedded highVoltageThreshold;

    public StaticVarCompensatorModificationEntity(@NonNull StaticVarCompensatorModificationInfos modificationInfos) {
        super(modificationInfos);
        assignAttributes(modificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((StaticVarCompensatorModificationInfos) modificationInfos);
    }

    private void assignAttributes(StaticVarCompensatorModificationInfos modificationInfos) {
        this.maxSusceptance = modificationInfos.getMaxSusceptance() != null ? new DoubleModificationEmbedded(modificationInfos.getMaxSusceptance()) : null;
        this.minSusceptance = modificationInfos.getMinSusceptance() != null ? new DoubleModificationEmbedded(modificationInfos.getMinSusceptance()) : null;
        this.maxQAtNominalV = modificationInfos.getMaxQAtNominalV() != null ? new DoubleModificationEmbedded(modificationInfos.getMaxQAtNominalV()) : null;
        this.minQAtNominalV = modificationInfos.getMinQAtNominalV() != null ? new DoubleModificationEmbedded(modificationInfos.getMinQAtNominalV()) : null;
        this.regulationMode = modificationInfos.getRegulationMode() != null ? new EnumModificationEmbedded<>(modificationInfos.getRegulationMode()) : null;
        this.regulating = modificationInfos.getRegulating() != null ? new BooleanModificationEmbedded(modificationInfos.getRegulating()) : null;
        this.voltageSetpoint = modificationInfos.getVoltageSetpoint() != null ? new DoubleModificationEmbedded(modificationInfos.getVoltageSetpoint()) : null;
        this.reactivePowerSetpoint = modificationInfos.getReactivePowerSetpoint() != null ? new DoubleModificationEmbedded(modificationInfos.getReactivePowerSetpoint()) : null;
        this.voltageRegulationType = modificationInfos.getVoltageRegulationType() != null ? new EnumModificationEmbedded<>(modificationInfos.getVoltageRegulationType()) : null;
        this.regulatingTerminalId = modificationInfos.getRegulatingTerminalId() != null ? new StringModificationEmbedded(modificationInfos.getRegulatingTerminalId()) : null;
        this.regulatingTerminalType = modificationInfos.getRegulatingTerminalType() != null ? new StringModificationEmbedded(modificationInfos.getRegulatingTerminalType()) : null;
        this.regulatingTerminalVlId = modificationInfos.getRegulatingTerminalVlId() != null ? new StringModificationEmbedded(modificationInfos.getRegulatingTerminalVlId()) : null;
        this.standbyAutomatonOn = modificationInfos.getStandbyAutomatonOn() != null ? new BooleanModificationEmbedded(modificationInfos.getStandbyAutomatonOn()) : null;
        this.standby = modificationInfos.getStandby() != null ? new BooleanModificationEmbedded(modificationInfos.getStandby()) : null;
        this.b0 = modificationInfos.getB0() != null ? new DoubleModificationEmbedded(modificationInfos.getB0()) : null;
        this.q0 = modificationInfos.getQ0() != null ? new DoubleModificationEmbedded(modificationInfos.getQ0()) : null;
        this.lowVoltageSetpoint = modificationInfos.getLowVoltageSetpoint() != null ? new DoubleModificationEmbedded(modificationInfos.getLowVoltageSetpoint()) : null;
        this.highVoltageSetpoint = modificationInfos.getHighVoltageSetpoint() != null ? new DoubleModificationEmbedded(modificationInfos.getHighVoltageSetpoint()) : null;
        this.lowVoltageThreshold = modificationInfos.getLowVoltageThreshold() != null ? new DoubleModificationEmbedded(modificationInfos.getLowVoltageThreshold()) : null;
        this.highVoltageThreshold = modificationInfos.getHighVoltageThreshold() != null ? new DoubleModificationEmbedded(modificationInfos.getHighVoltageThreshold()) : null;
    }

    @Override
    public StaticVarCompensatorModificationInfos toModificationInfos() {
        return StaticVarCompensatorModificationInfos.builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .description(getDescription())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connectionName(toAttributeModification(getConnectionName()))
                .connectionDirection(toAttributeModification(getConnectionDirection()))
                .connectionPosition(toAttributeModification(getConnectionPosition()))
                .terminalConnected(toAttributeModification(getTerminalConnected()))
                .pMeasurementValue(toAttributeModification(getPMeasurementValue()))
                .pMeasurementValidity(toAttributeModification(getPMeasurementValidity()))
                .qMeasurementValue(toAttributeModification(getQMeasurementValue()))
                .qMeasurementValidity(toAttributeModification(getQMeasurementValidity()))
                .maxSusceptance(toAttributeModification(getMaxSusceptance()))
                .minSusceptance(toAttributeModification(getMinSusceptance()))
                .maxQAtNominalV(toAttributeModification(getMaxQAtNominalV()))
                .minQAtNominalV(toAttributeModification(getMinQAtNominalV()))
                .regulationMode(toAttributeModification(getRegulationMode()))
                .regulating(toAttributeModification(getRegulating()))
                .voltageSetpoint(toAttributeModification(getVoltageSetpoint()))
                .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
                .voltageRegulationType(toAttributeModification(getVoltageRegulationType()))
                .regulatingTerminalId(toAttributeModification(getRegulatingTerminalId()))
                .regulatingTerminalType(toAttributeModification(getRegulatingTerminalType()))
                .regulatingTerminalVlId(toAttributeModification(getRegulatingTerminalVlId()))
                .standbyAutomatonOn(toAttributeModification(getStandbyAutomatonOn()))
                .standby(toAttributeModification(getStandby()))
                .b0(toAttributeModification(getB0()))
                .q0(toAttributeModification(getQ0()))
                .lowVoltageSetpoint(toAttributeModification(getLowVoltageSetpoint()))
                .highVoltageSetpoint(toAttributeModification(getHighVoltageSetpoint()))
                .lowVoltageThreshold(toAttributeModification(getLowVoltageThreshold()))
                .highVoltageThreshold(toAttributeModification(getHighVoltageThreshold()))
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList())
                .build();
    }
}
