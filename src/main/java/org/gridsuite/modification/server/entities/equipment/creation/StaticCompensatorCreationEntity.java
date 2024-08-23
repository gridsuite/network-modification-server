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
import org.gridsuite.modification.server.dto.StandByAutomatonCreationInfos;
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
    private Double voltageSetpoint;

    @Column
    private Double reactivePowerSetpoint;

    @Column(name = "regulatingTerminalId")
    private String regulatingTerminalId;

    @Enumerated(EnumType.STRING)
    @Column(name = "voltageRegulationType")
    private VoltageRegulationType voltageRegulationType;

    @Enumerated(EnumType.STRING)
    @Column(name = "regulationMode")
    private StaticVarCompensator.RegulationMode regulationMode;

    @Column(name = "regulatingTerminalVlId")
    private String regulatingTerminalVlId;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "standBy_automaton_creation",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "standBy_automaton_creation_id_fk"
            ))
    private StandByAutomatonCreationEntity standByAutomatonCreation;

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
        this.regulationMode = creationInfos.getRegulationMode();
        this.voltageSetpoint = creationInfos.getVoltageSetpoint();
        this.reactivePowerSetpoint = creationInfos.getReactivePowerSetpoint();
        this.regulatingTerminalId = creationInfos.getRegulatingTerminalId();
        this.voltageRegulationType = creationInfos.getVoltageRegulationType();
        this.regulatingTerminalVlId = creationInfos.getRegulatingTerminalVlId();
        this.standByAutomatonCreation = toEmbeddableStandByAutomaton(creationInfos.getStandbyAutomatonCreationInfos());
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
            .regulationMode(getRegulationMode())
            .reactivePowerSetpoint(getReactivePowerSetpoint())
            .voltageSetpoint(getVoltageSetpoint())
            .voltageRegulationType(getVoltageRegulationType())
            .regulatingTerminalId(getRegulatingTerminalId())
            .regulatingTerminalVlId(getRegulatingTerminalVlId())
            .standbyAutomatonCreationInfos(toStandByAutomatonInfos(getStandByAutomatonCreation()))
             // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }

    private static StandByAutomatonCreationEntity toEmbeddableStandByAutomaton(
            StandByAutomatonCreationInfos standByAutomaton) {
        return standByAutomaton == null ? null :
                 StandByAutomatonCreationEntity.builder()
                         .standby(standByAutomaton.isStandby())
                         .b0(standByAutomaton.getB0())
                         .lowVoltageSetpoint(standByAutomaton.getLowVoltageSetpoint())
                         .highVoltageSetpoint(standByAutomaton.getHighVoltageSetpoint())
                         .lowVoltageThreshold(standByAutomaton.getLowVoltageThreshold())
                         .highVoltageThreshold(standByAutomaton.getHighVoltageThreshold())
                         .build();
    }

    private static StandByAutomatonCreationInfos toStandByAutomatonInfos(StandByAutomatonCreationEntity standByAutomatonEmbeddable) {
        if (standByAutomatonEmbeddable == null) {
            return null;
        }

        return StandByAutomatonCreationInfos.builder()
                        .b0(standByAutomatonEmbeddable.getB0())
                        .standby(standByAutomatonEmbeddable.isStandby())
                        .highVoltageSetpoint(standByAutomatonEmbeddable.getHighVoltageSetpoint())
                        .lowVoltageSetpoint(standByAutomatonEmbeddable.getLowVoltageSetpoint())
                        .highVoltageThreshold(standByAutomatonEmbeddable.getHighVoltageThreshold())
                        .lowVoltageSetpoint(standByAutomatonEmbeddable.getLowVoltageThreshold())
                        .build();
    }
}
