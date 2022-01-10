/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.EnergySource;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "generatorCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "generatorCreation_id_fk_constraint"))
public class GeneratorCreationEntity extends InjectionCreationEntity {
    @Column(name = "energySource")
    private EnergySource energySource;

    @Column(name = "minActivePower")
    private double minActivePower;

    @Column(name = "maxActivePower")
    private double maxActivePower;

    @Column(name = "ratedNominalPower")
    private Double ratedNominalPower;

    @Column(name = "activePowerSetpoint")
    private double activePowerSetpoint;

    @Column(name = "reactivePowerSetpoint")
    private Double reactivePowerSetpoint;

    @Column(name = "voltageRegulationOn")
    private boolean voltageRegulationOn;

    @Column(name = "voltageSetpoint")
    private Double voltageSetpoint;

    public GeneratorCreationEntity(String equipmentId, String equipmentName, EnergySource energySource, String voltageLevelId, String busOrBusbarSectionId,
                                   double minActivePower, double maxActivePower, Double ratedNominalPower, double activePowerSetpoint,
                                   Double reactivePowerSetpoint, boolean voltageRegulationOn, Double voltageSetpoint) {
        super(ModificationType.GENERATOR_CREATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.energySource = energySource;
        this.minActivePower = minActivePower;
        this.maxActivePower = maxActivePower;
        this.ratedNominalPower = ratedNominalPower;
        this.activePowerSetpoint = activePowerSetpoint;
        this.reactivePowerSetpoint = reactivePowerSetpoint;
        this.voltageRegulationOn = voltageRegulationOn;
        this.voltageSetpoint = voltageSetpoint;
    }

    @Override
    public GeneratorCreationInfos toModificationInfos() {
        return toGeneratorCreationInfosBuilder().build();
    }

    @Override
    public EquipmenModificationInfos toEquipmentModificationInfos(Set<String> uuids) {
        return toGeneratorCreationInfosBuilder().substationIds(uuids).build();
    }

    private GeneratorCreationInfos.GeneratorCreationInfosBuilder<?, ?> toGeneratorCreationInfosBuilder() {
        return GeneratorCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .energySource(getEnergySource())
            .minActivePower(getMinActivePower())
            .maxActivePower(getMaxActivePower())
            .ratedNominalPower(getRatedNominalPower())
            .activePowerSetpoint(getActivePowerSetpoint())
            .reactivePowerSetpoint(getReactivePowerSetpoint())
            .voltageRegulationOn(isVoltageRegulationOn())
            .voltageSetpoint(getVoltageSetpoint());
    }
}
