/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

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

    @Column
    private Double voltageSetpoint;

    @Column
    private Double reactivePowerSetpoint;

    @Column(name = "regulatingTerminalId")
    private String regulatingTerminalId;

    @Enumerated(EnumType.STRING)
    @Column(name = "voltageRegulationType")
    private VoltageRegulationType voltageRegulationType;

    @Column(name = "regulatingTerminalVlId")
    private String regulatingTerminalVlId;

    @ElementCollection
    @CollectionTable(name = "standBy_automaton_creation")
    private List<StandByAutomatonCreationEmbeddable> standByAutomatonCreationEmbeddable;

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
        this.voltageSetpoint = creationInfos.getVoltageSetpoint();
        this.reactivePowerSetpoint = creationInfos.getReactivePowerSetpoint();
        this.regulatingTerminalId = creationInfos.getRegulatingTerminalId();
        this.voltageRegulationType = creationInfos.getVoltageRegulationType();
        this.regulatingTerminalVlId = creationInfos.getRegulatingTerminalVlId();
        this.standByAutomatonCreationEmbeddable = toEmbeddableStandByAutomaton(creationInfos.getStandbyAutomatonCreationInfos());
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
            .connected(isConnected())
            .maxSusceptance(getMaxSusceptance())
            .minSusceptance(getMinSusceptance())
            .maxQAtNominalV(getMaxQAtNominalV())
            .minQAtNominalV(getMinQAtNominalV())
            .reactivePowerSetpoint(getReactivePowerSetpoint())
            .voltageSetpoint(getVoltageSetpoint())
            .voltageRegulationType(getVoltageRegulationType())
            .regulatingTerminalId(getRegulatingTerminalId())
            .regulatingTerminalVlId(getRegulatingTerminalVlId())
            .standbyAutomatonCreationInfos(toStandByAutomatonInfos(getStandByAutomatonCreationEmbeddable()))
             // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }

    private static List<StandByAutomatonCreationEmbeddable> toEmbeddableStandByAutomaton(
            List<StandByAutomatonCreationInfos> standByAutomaton) {
        return standByAutomaton == null ? null : standByAutomaton.stream()
                .map(automaton -> new StandByAutomatonCreationEmbeddable(automaton.isStandby(),
                        automaton.getB0(),
                        automaton.getQ0(),
                        automaton.getLowVoltageSetpoint(),
                        automaton.getHighVoltageSetpoint(),
                        automaton.getLowVoltageThreshold(),
                        automaton.getHighVoltageThreshold()))
                .collect(Collectors.toList());
    }

    private static List<StandByAutomatonCreationInfos> toStandByAutomatonInfos(List<StandByAutomatonCreationEmbeddable> standByAutomatonEmbeddable) {
        if (standByAutomatonEmbeddable == null) {
            return null;
        }

        return standByAutomatonEmbeddable.stream()
                .map(automatonEmbeddable -> StandByAutomatonCreationInfos.builder()
                        .b0(automatonEmbeddable.getB0())
                        .q0(automatonEmbeddable.getQ0())
                        .standby(automatonEmbeddable.isStandby())
                        .highVoltageSetpoint(automatonEmbeddable.getHighVoltageSetpoint())
                        .lowVoltageSetpoint(automatonEmbeddable.getLowVoltageSetpoint())
                        .highVoltageThreshold(automatonEmbeddable.getHighVoltageThreshold())
                        .lowVoltageSetpoint(automatonEmbeddable.getLowVoltageThreshold())
                        .build())
                .collect(Collectors.toList());
    }
}
