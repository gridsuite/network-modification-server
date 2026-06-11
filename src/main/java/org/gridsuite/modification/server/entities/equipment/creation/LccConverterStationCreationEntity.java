/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.model.LccConverterStationCreationModel;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEmbeddable.fromEmbeddableShuntCompensatorCreation;
import static org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEmbeddable.toEmbeddableShuntCompensatorCreation;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lccConverterStationCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lcc_converter_station_creation_id_fk_constraint"))
public class LccConverterStationCreationEntity extends InjectionCreationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private Float lossFactor;

    @Column
    private Float powerFactor;

    @ElementCollection
    @CollectionTable(name = "shunt_compensator_on_side", indexes = {@Index(name = "shunt_compensator_on_side_index", columnList = "lcc_converter_station_creation_entity_id")},
            foreignKey = @ForeignKey(name = "lcc_converter_station_creation_shunt_compensators_on_side_fk"))
    private List<ShuntCompensatorCreationEmbeddable> shuntCompensatorsOnSide;

    public LccConverterStationCreationEntity(ModificationInfos converterStationCreationInfos) {
        super(converterStationCreationInfos);
        assignAttributes((LccConverterStationCreationModel) converterStationCreationInfos);
    }

    public LccConverterStationCreationEntity(LccConverterStationCreationModel converterStationCreationModel) {
        super(converterStationCreationModel);
        assignAttributes(converterStationCreationModel);
    }

    private void assignAttributes(LccConverterStationCreationModel converterStationCreationInfos) {
        this.lossFactor = converterStationCreationInfos.getLossFactor();
        this.powerFactor = converterStationCreationInfos.getPowerFactor();
        this.shuntCompensatorsOnSide = toEmbeddableShuntCompensatorCreation(converterStationCreationInfos.getShuntCompensatorsOnSide());
    }

    public LccConverterStationCreationInfos toLccConverterStationInfos() {
        return LccConverterStationCreationInfos.builder()
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                // Injection
                .voltageLevelId(getVoltageLevelId())
                .busOrBusbarSectionId(getBusOrBusbarSectionId())
                .connectionName(getConnectionName())
                .connectionPosition(getConnectionPosition())
                .connectionDirection(getConnectionDirection())
                .terminalConnected(isTerminalConnected())
                // ConverterStation
                .lossFactor(getLossFactor())
                .powerFactor(getPowerFactor())
                .shuntCompensatorsOnSide(fromEmbeddableShuntCompensatorCreation(getShuntCompensatorsOnSide()))
                .build();
    }
}
