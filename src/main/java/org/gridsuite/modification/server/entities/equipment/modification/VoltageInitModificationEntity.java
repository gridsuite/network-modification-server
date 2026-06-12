/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageInitModificationInfos;
import org.gridsuite.modification.model.VoltageInitBusModificationModel;
import org.gridsuite.modification.model.VoltageInitGeneratorModificationModel;
import org.gridsuite.modification.model.VoltageInitShuntCompensatorModificationModel;
import org.gridsuite.modification.model.VoltageInitStaticVarCompensatorModificationModel;
import org.gridsuite.modification.model.VoltageInitTransformerModificationModel;
import org.gridsuite.modification.model.VoltageInitVscConverterStationModificationModel;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "voltageInitModification")
public class VoltageInitModificationEntity extends ModificationEntity {
    @ElementCollection
    @CollectionTable(name = "voltageInitGeneratorsModification",
        indexes = {@Index(name = "VoltageInitModificationEntity_generators_idx1", columnList = "voltage_init_modification_entity_id")},
        foreignKey = @ForeignKey(name = "VoltageInitModificationEntity_generators_fk1"))
    private List<VoltageInitGeneratorModificationEmbeddable> generators;

    @ElementCollection
    @CollectionTable(name = "voltageInitTransformersModification",
        indexes = {@Index(name = "VoltageInitModificationEntity_transformers_idx1", columnList = "voltage_init_modification_entity_id")},
        foreignKey = @ForeignKey(name = "VoltageInitModificationEntity_transformers_fk1"))
    private List<VoltageInitTransformerModificationEmbeddable> transformers;

    @ElementCollection
    @CollectionTable(name = "voltageInitStaticVarCompensatorsModification",
        indexes = {@Index(name = "VoltageInitModificationEntity_static_var_compensators_idx1", columnList = "voltage_init_modification_entity_id")},
        foreignKey = @ForeignKey(name = "VoltageInitModificationEntity_static_var_compensators_fk1"))
    private List<VoltageInitStaticVarCompensatorModificationEmbeddable> staticVarCompensators;

    @ElementCollection
    @CollectionTable(name = "voltageInitVscConverterStationsModification",
        indexes = {@Index(name = "VoltageInitModificationEntity_vsc_converter_stations_idx1", columnList = "voltage_init_modification_entity_id")},
        foreignKey = @ForeignKey(name = "VoltageInitModificationEntity_vsc_converter_stations_fk1"))
    private List<VoltageInitVscConverterStationModificationEmbeddable> vscConverterStations;

    @ElementCollection
    @CollectionTable(name = "voltageInitShuntCompensatorsModification",
        indexes = {@Index(name = "VoltageInitModificationEntity_shunt_compensators_idx1", columnList = "voltage_init_modification_entity_id")},
        foreignKey = @ForeignKey(name = "VoltageInitModificationEntity_shunt_compensators_fk1"))
    private List<VoltageInitShuntCompensatorModificationEmbeddable> shuntCompensators;

    @ElementCollection
    @CollectionTable(name = "voltageInitBusModification",
        indexes = {@Index(name = "VoltageInitModificationEntity_buses_idx1", columnList = "voltage_init_modification_entity_id")},
        foreignKey = @ForeignKey(name = "VoltageInitModificationEntity_buses_fk1"))
    private List<VoltageInitBusModificationEmbeddable> buses;

    @Column(name = "rootNetworkName")
    private String rootNetworkName;

    @Column(name = "nodeName")
    private String nodeName;

    @Column(name = "computationDate", columnDefinition = "timestamptz")
    private Instant computationDate;

    public VoltageInitModificationEntity(VoltageInitModificationInfos voltageInitModificationInfos) {
        super(voltageInitModificationInfos);
        assignAttributes(voltageInitModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos voltageInitModificationInfos) {
        super.update(voltageInitModificationInfos);
        assignAttributes((VoltageInitModificationInfos) voltageInitModificationInfos);
    }

    private void assignAttributes(VoltageInitModificationInfos voltageInitModificationInfos) {
        generators = toEmbeddableVoltageInitGenerators(voltageInitModificationInfos.getGenerators());
        transformers = toEmbeddableVoltageInitTransformers(voltageInitModificationInfos.getTransformers());
        staticVarCompensators = toEmbeddableVoltageInitStaticVarCompensators(voltageInitModificationInfos.getStaticVarCompensators());
        vscConverterStations = toEmbeddableVoltageInitVscConverterStations(voltageInitModificationInfos.getVscConverterStations());
        shuntCompensators = toEmbeddableVoltageInitShuntCompensators(voltageInitModificationInfos.getShuntCompensators());
        buses = toEmbeddableVoltageInitBuses(voltageInitModificationInfos.getBuses());
        rootNetworkName = voltageInitModificationInfos.getRootNetworkName();
        nodeName = voltageInitModificationInfos.getNodeName();
        //We need to limit the precision to avoid database precision storage limit issue (postgres has a precision of 6 digits while h2 can go to 9)
        this.computationDate = voltageInitModificationInfos.getComputationDate() != null ? voltageInitModificationInfos.getComputationDate().truncatedTo(ChronoUnit.MICROS) : null;
    }

    public static List<VoltageInitGeneratorModificationEmbeddable> toEmbeddableVoltageInitGenerators(List<VoltageInitGeneratorModificationModel> generators) {
        return generators == null ? null : generators.stream()
            .map(generator -> new VoltageInitGeneratorModificationEmbeddable(generator.getGeneratorId(), generator.getTargetV(), generator.getTargetQ()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitGeneratorModificationModel> toGeneratorsModification(List<VoltageInitGeneratorModificationEmbeddable> generators) {
        return generators != null ? generators
            .stream()
            .map(generator -> new VoltageInitGeneratorModificationModel(generator.getGeneratorId(), generator.getTargetV(), generator.getTargetQ()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitTransformerModificationEmbeddable> toEmbeddableVoltageInitTransformers(List<VoltageInitTransformerModificationModel> transformers) {
        return transformers == null ? null : transformers.stream()
            .map(transformer -> new VoltageInitTransformerModificationEmbeddable(transformer.getTransformerId(), transformer.getRatioTapChangerPosition(),
                    transformer.getRatioTapChangerTargetV(), transformer.getLegSide()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitTransformerModificationModel> toTransformersModification(List<VoltageInitTransformerModificationEmbeddable> transformers) {
        return transformers != null ? transformers
            .stream()
            .map(transformer -> new VoltageInitTransformerModificationModel(transformer.getTransformerId(), transformer.getRatioTapChangerPosition(),
                    transformer.getRatioTapChangerTargetV(), transformer.getLegSide()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitStaticVarCompensatorModificationEmbeddable> toEmbeddableVoltageInitStaticVarCompensators(
            List<VoltageInitStaticVarCompensatorModificationModel> staticVarCompensators) {
        return staticVarCompensators == null ? null : staticVarCompensators.stream()
            .map(staticVarCompensator -> new VoltageInitStaticVarCompensatorModificationEmbeddable(staticVarCompensator.getStaticVarCompensatorId(),
                    staticVarCompensator.getVoltageSetpoint(), staticVarCompensator.getReactivePowerSetpoint()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitStaticVarCompensatorModificationModel> toStaticVarCompensatorsModification(List<VoltageInitStaticVarCompensatorModificationEmbeddable> staticVarCompensators) {
        return staticVarCompensators != null ? staticVarCompensators
            .stream()
            .map(staticVarCompensator -> new VoltageInitStaticVarCompensatorModificationModel(staticVarCompensator.getStaticVarCompensatorId(),
                    staticVarCompensator.getVoltageSetpoint(), staticVarCompensator.getReactivePowerSetpoint()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitVscConverterStationModificationEmbeddable> toEmbeddableVoltageInitVscConverterStations(List<VoltageInitVscConverterStationModificationModel> vscConverterStations) {
        return vscConverterStations == null ? null : vscConverterStations.stream()
            .map(vscConverterStation -> new VoltageInitVscConverterStationModificationEmbeddable(vscConverterStation.getVscConverterStationId(),
                    vscConverterStation.getVoltageSetpoint(), vscConverterStation.getReactivePowerSetpoint()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitVscConverterStationModificationModel> toVscConverterStationsModification(List<VoltageInitVscConverterStationModificationEmbeddable> vscConverterStations) {
        return vscConverterStations != null ? vscConverterStations
            .stream()
            .map(vscConverterStation -> new VoltageInitVscConverterStationModificationModel(vscConverterStation.getVscConverterStationId(),
                    vscConverterStation.getVoltageSetpoint(), vscConverterStation.getReactivePowerSetpoint()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitShuntCompensatorModificationEmbeddable> toEmbeddableVoltageInitShuntCompensators(List<VoltageInitShuntCompensatorModificationModel> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
            .map(shuntCompensator -> new VoltageInitShuntCompensatorModificationEmbeddable(shuntCompensator.getShuntCompensatorId(),
                    shuntCompensator.getSectionCount(), shuntCompensator.getConnect(), shuntCompensator.getTargetV()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitShuntCompensatorModificationModel> toShuntCompensatorsModification(List<VoltageInitShuntCompensatorModificationEmbeddable> shuntCompensators) {
        return shuntCompensators != null ? shuntCompensators
            .stream()
            .map(shuntCompensator -> new VoltageInitShuntCompensatorModificationModel(shuntCompensator.getShuntCompensatorId(),
                    shuntCompensator.getSectionCount(), shuntCompensator.getConnect(), shuntCompensator.getTargetV()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitBusModificationEmbeddable> toEmbeddableVoltageInitBuses(List<VoltageInitBusModificationModel> buses) {
        return buses == null ? null : buses.stream()
            .map(bus -> new VoltageInitBusModificationEmbeddable(bus.getVoltageLevelId(), bus.getBusId(), bus.getV(), bus.getAngle()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitBusModificationModel> toBusesModification(List<VoltageInitBusModificationEmbeddable> buses) {
        return buses != null ? buses
            .stream()
            .map(bus -> new VoltageInitBusModificationModel(bus.getVoltageLevelId(), bus.getBusId(), bus.getV(), bus.getAngle()))
            .collect(Collectors.toList()) : null;
    }

    @Override
    public VoltageInitModificationInfos toModificationInfos() {
        return VoltageInitModificationInfos.builder()
            .date(getDate())
            .uuid(getId())
            .stashed(getStashed())
            .activated(getActivated())
            .description(getDescription())
            .generators(toGeneratorsModification(generators))
            .transformers(toTransformersModification(transformers))
            .staticVarCompensators(toStaticVarCompensatorsModification(staticVarCompensators))
            .vscConverterStations(toVscConverterStationsModification(vscConverterStations))
            .shuntCompensators(toShuntCompensatorsModification(shuntCompensators))
            .buses(toBusesModification(buses))
            .rootNetworkName(getRootNetworkName())
            .nodeName(getNodeName())
            .computationDate(getComputationDate())
            .build();
    }
}
