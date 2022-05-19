/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.EnergySource;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;


/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "generatorModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "generatorModification_id_fk_constraint"))
public class GeneratorModificationEntity extends InjectionModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "energySource")),
        @AttributeOverride(name = "opType", column = @Column(name = "energySourceOp"))
    })
    EnumModificationEmbedded<EnergySource> energySource;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minActivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "minActivePowerOp"))
    })
    DoubleModificationEmbedded minActivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxActivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxActivePowerOp"))
    })
    DoubleModificationEmbedded maxActivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedNominalPower")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedNominalPowerOp"))
    })
    DoubleModificationEmbedded ratedNominalPower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "activePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "activePowerSetpointOp"))
    })
    DoubleModificationEmbedded activePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "reactivePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerSetpointOp"))
    })
    DoubleModificationEmbedded reactivePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationOn")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationOnOp"))
    })
    BooleanModificationEmbedded voltageRegulationOn;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageSetpointOp"))
    })
    DoubleModificationEmbedded voltageSetpoint;

    public GeneratorModificationEntity(String equipmentId, AttributeModification<String> equipmentName, AttributeModification<String> voltageLevelId, AttributeModification<String> busOrBusbarSectionId,
                                       AttributeModification<EnergySource> energySource,
                                       AttributeModification<Double> minActivePower, AttributeModification<Double> maxActivePower, AttributeModification<Double> ratedNominalPower,
                                       AttributeModification<Double> activePowerSetpoint, AttributeModification<Double> reactivePowerSetpoint, AttributeModification<Boolean> voltageRegulationOn, AttributeModification<Double> voltageSetpoint) {
        super(ModificationType.GENERATOR_MODIFICATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.energySource = new EnumModificationEmbedded<>(energySource);
        this.minActivePower = new DoubleModificationEmbedded(minActivePower);
        this.maxActivePower = new DoubleModificationEmbedded(maxActivePower);
        this.ratedNominalPower = new DoubleModificationEmbedded(ratedNominalPower);
        this.activePowerSetpoint = new DoubleModificationEmbedded(activePowerSetpoint);
        this.reactivePowerSetpoint = new DoubleModificationEmbedded(reactivePowerSetpoint);
        this.voltageRegulationOn = new BooleanModificationEmbedded(voltageRegulationOn);
        this.voltageSetpoint = new DoubleModificationEmbedded(voltageSetpoint);
    }

    public GeneratorModificationEntity(GeneratorModificationInfos infos) {
        this(infos.getEquipmentId(), infos.getEquipmentName(), infos.getVoltageLevelId(), infos.getBusOrBusbarSectionId(),
            infos.getEnergySource(),
            infos.getMinActivePower(), infos.getMaxActivePower(), infos.getRatedNominalPower(),
            infos.getActivePowerSetpoint(), infos.getReactivePowerSetpoint(), infos.getVoltageRegulationOn(), infos.getVoltageSetpoint());
    }

    @Override
    public GeneratorModificationInfos toModificationInfos() {
        return toGeneratorModificationInfosBuilder().build();
    }

    private GeneratorModificationInfos.GeneratorModificationInfosBuilder<?, ?> toGeneratorModificationInfosBuilder() {
        return GeneratorModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(new AttributeModification<>(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(new AttributeModification<>(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(new AttributeModification<>(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
            .energySource(toAttributeModification(getEnergySource()))
            .activePowerSetpoint(toAttributeModification(getActivePowerSetpoint()))
            .maxActivePower(toAttributeModification(getMaxActivePower()))
            .minActivePower(toAttributeModification(getMinActivePower()))
            .ratedNominalPower(toAttributeModification(getRatedNominalPower()))
            .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
            .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
            .type(ModificationType.GENERATOR_MODIFICATION)
            .voltageSetpoint(toAttributeModification(getVoltageSetpoint()));
    }
}
