/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.StaticVarCompensator;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.StaticVarCompensatorCreationInfos;
import org.gridsuite.modification.server.dto.VoltageRegulationType;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "staticVarCompensatorCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "staticVarCompensatorCreation_id_fk_constraint"))
public class StaticCompensatorCreationEntity extends InjectionCreationEntity {
    @Column
    private Double maxSusceptance;

    @Column
    private Double minSusceptance;

    @Column
    private Double maxQAtNominalV;

    @Column
    private Double minQAtNominalV;

    @Enumerated(EnumType.STRING)
    @Column(name = "regulationMode")
    private StaticVarCompensator.RegulationMode regulationMode;

    @Column
    private Double voltageSetpoint;

    @Column
    private Double reactivePowerSetpoint;

    @Enumerated(EnumType.STRING)
    @Column(name = "voltageRegulationType")
    private VoltageRegulationType voltageRegulationType;

    @Column(name = "regulatingTerminalId")
    private String regulatingTerminalId;

    @Column(name = "regulatingTerminalType")
    private String regulatingTerminalType;

    @Column(name = "regulatingTerminalVlId")
    private String regulatingTerminalVlId;

    @Column
    private boolean standbyAutomatonOn;

    @Column
    private boolean standby;

    @Column
    private Double b0;

    @Column
    private Double q0;

    @Column
    private Double lowVoltageSetpoint;

    @Column
    private Double highVoltageSetpoint;

    @Column
    private Double lowVoltageThreshold;

    @Column
    private Double highVoltageThreshold;

    public StaticCompensatorCreationEntity(StaticVarCompensatorCreationInfos creationInfos) {
        super(creationInfos);
        assignAttributes(creationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos = (StaticVarCompensatorCreationInfos) modificationInfos;
        assignAttributes(staticVarCompensatorCreationInfos);
    }

    private void assignAttributes(StaticVarCompensatorCreationInfos creationInfos) {
        this.maxSusceptance = creationInfos.getMaxSusceptance();
        this.minSusceptance = creationInfos.getMinSusceptance();
        this.maxQAtNominalV = creationInfos.getMaxQAtNominalV();
        this.minQAtNominalV = creationInfos.getMinQAtNominalV();
        this.regulationMode = creationInfos.getRegulationMode();
        this.voltageSetpoint = creationInfos.getVoltageSetpoint();
        this.reactivePowerSetpoint = creationInfos.getReactivePowerSetpoint();
        this.voltageRegulationType = creationInfos.getVoltageRegulationType();
        this.regulatingTerminalId = creationInfos.getRegulatingTerminalId();
        this.regulatingTerminalType = creationInfos.getRegulatingTerminalType();
        this.regulatingTerminalVlId = creationInfos.getRegulatingTerminalVlId();
        this.standbyAutomatonOn = creationInfos.isStandbyAutomatonOn();
        this.standby = creationInfos.isStandby();
        this.b0 = creationInfos.getB0();
        this.q0 = creationInfos.getQ0();
        this.highVoltageSetpoint = creationInfos.getHighVoltageSetpoint();
        this.lowVoltageSetpoint = creationInfos.getLowVoltageSetpoint();
        this.lowVoltageThreshold = creationInfos.getLowVoltageThreshold();
        this.highVoltageThreshold = creationInfos.getHighVoltageThreshold();
    }

    @Override
    public StaticVarCompensatorCreationInfos toModificationInfos() {
        return toStaticVarCompensatorCreationInfosBuilder().build();
    }

    private StaticVarCompensatorCreationInfos.StaticVarCompensatorCreationInfosBuilder<?, ?> toStaticVarCompensatorCreationInfosBuilder() {
        return StaticVarCompensatorCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            // Injection
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .connectionName(getConnectionName())
            .connectionDirection(getConnectionDirection())
            .connectionPosition(getConnectionPosition())
            .terminalConnected(isTerminalConnected())
            .maxSusceptance(getMaxSusceptance())
            .minSusceptance(getMinSusceptance())
            .minQAtNominalV(getMinQAtNominalV())
            .maxQAtNominalV(getMaxQAtNominalV())
            .regulationMode(getRegulationMode())
            .reactivePowerSetpoint(getReactivePowerSetpoint())
            .voltageSetpoint(getVoltageSetpoint())
            .voltageRegulationType(getVoltageRegulationType())
            .regulatingTerminalId(getRegulatingTerminalId())
            .regulatingTerminalType(getRegulatingTerminalType())
            .regulatingTerminalVlId(getRegulatingTerminalVlId())
            // Standby automaton
            .standbyAutomatonOn(isStandbyAutomatonOn())
            .standby(isStandby())
            .b0(getB0())
            .q0(getQ0())
            .lowVoltageSetpoint(getLowVoltageSetpoint())
            .highVoltageSetpoint(getHighVoltageSetpoint())
            .lowVoltageThreshold(getLowVoltageThreshold())
            .highVoltageThreshold(getHighVoltageThreshold())
             // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
