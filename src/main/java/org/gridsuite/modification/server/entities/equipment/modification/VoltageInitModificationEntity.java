/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.dto.VoltageInitGeneratorModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitBusModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitStaticVarCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitTransformerModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitVscConverterStationModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
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

    }

    public static List<VoltageInitGeneratorModificationEmbeddable> toEmbeddableVoltageInitGenerators(List<VoltageInitGeneratorModificationInfos> generators) {
        return generators == null ? null : generators.stream()
            .map(generator -> new VoltageInitGeneratorModificationEmbeddable(generator.getGeneratorId(), generator.getTargetV(), generator.getTargetQ()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitGeneratorModificationInfos> toGeneratorsModification(List<VoltageInitGeneratorModificationEmbeddable> generators) {
        return generators != null ? generators
            .stream()
            .map(generator -> new VoltageInitGeneratorModificationInfos(generator.getGeneratorId(), generator.getTargetV(), generator.getTargetQ()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitTransformerModificationEmbeddable> toEmbeddableVoltageInitTransformers(List<VoltageInitTransformerModificationInfos> transformers) {
        return transformers == null ? null : transformers.stream()
            .map(transformer -> new VoltageInitTransformerModificationEmbeddable(transformer.getTransformerId(), transformer.getRatioTapChangerPosition(), transformer.getRatioTapChangerTargetV(), transformer.getLegSide()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitTransformerModificationInfos> toTransformersModification(List<VoltageInitTransformerModificationEmbeddable> transformers) {
        return transformers != null ? transformers
            .stream()
            .map(transformer -> new VoltageInitTransformerModificationInfos(transformer.getTransformerId(), transformer.getRatioTapChangerPosition(), transformer.getRatioTapChangerTargetV(), transformer.getLegSide()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitStaticVarCompensatorModificationEmbeddable> toEmbeddableVoltageInitStaticVarCompensators(List<VoltageInitStaticVarCompensatorModificationInfos> staticVarCompensators) {
        return staticVarCompensators == null ? null : staticVarCompensators.stream()
            .map(staticVarCompensator -> new VoltageInitStaticVarCompensatorModificationEmbeddable(staticVarCompensator.getStaticVarCompensatorId(), staticVarCompensator.getVoltageSetpoint(), staticVarCompensator.getReactivePowerSetpoint()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitStaticVarCompensatorModificationInfos> toStaticVarCompensatorsModification(List<VoltageInitStaticVarCompensatorModificationEmbeddable> staticVarCompensators) {
        return staticVarCompensators != null ? staticVarCompensators
            .stream()
            .map(staticVarCompensator -> new VoltageInitStaticVarCompensatorModificationInfos(staticVarCompensator.getStaticVarCompensatorId(), staticVarCompensator.getVoltageSetpoint(), staticVarCompensator.getReactivePowerSetpoint()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitVscConverterStationModificationEmbeddable> toEmbeddableVoltageInitVscConverterStations(List<VoltageInitVscConverterStationModificationInfos> vscConverterStations) {
        return vscConverterStations == null ? null : vscConverterStations.stream()
            .map(vscConverterStation -> new VoltageInitVscConverterStationModificationEmbeddable(vscConverterStation.getVscConverterStationId(), vscConverterStation.getVoltageSetpoint(), vscConverterStation.getReactivePowerSetpoint()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitVscConverterStationModificationInfos> toVscConverterStationsModification(List<VoltageInitVscConverterStationModificationEmbeddable> vscConverterStations) {
        return vscConverterStations != null ? vscConverterStations
            .stream()
            .map(vscConverterStation -> new VoltageInitVscConverterStationModificationInfos(vscConverterStation.getVscConverterStationId(), vscConverterStation.getVoltageSetpoint(), vscConverterStation.getReactivePowerSetpoint()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitShuntCompensatorModificationEmbeddable> toEmbeddableVoltageInitShuntCompensators(List<VoltageInitShuntCompensatorModificationInfos> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
            .map(shuntCompensator -> new VoltageInitShuntCompensatorModificationEmbeddable(shuntCompensator.getShuntCompensatorId(), shuntCompensator.getSectionCount(), shuntCompensator.getConnect(), shuntCompensator.getTargetV()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitShuntCompensatorModificationInfos> toShuntCompensatorsModification(List<VoltageInitShuntCompensatorModificationEmbeddable> shuntCompensators) {
        return shuntCompensators != null ? shuntCompensators
            .stream()
            .map(shuntCompensator -> new VoltageInitShuntCompensatorModificationInfos(shuntCompensator.getShuntCompensatorId(), shuntCompensator.getSectionCount(), shuntCompensator.getConnect(), shuntCompensator.getTargetV()))
            .collect(Collectors.toList()) : null;
    }

    public static List<VoltageInitBusModificationEmbeddable> toEmbeddableVoltageInitBuses(List<VoltageInitBusModificationInfos> buses) {
        return buses == null ? null : buses.stream()
            .map(bus -> new VoltageInitBusModificationEmbeddable(bus.getBusId(), bus.getV()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitBusModificationInfos> toBusesModification(List<VoltageInitBusModificationEmbeddable> buses) {
        return buses != null ? buses
            .stream()
            .map(bus -> new VoltageInitBusModificationInfos(bus.getBusId(), bus.getV()))
            .collect(Collectors.toList()) : null;
    }

    @Override
    public VoltageInitModificationInfos toModificationInfos() {
        return VoltageInitModificationInfos.builder()
            .date(getDate())
            .uuid(getId())
            .stashed(getStashed())
            .generators(toGeneratorsModification(generators))
            .transformers(toTransformersModification(transformers))
            .staticVarCompensators(toStaticVarCompensatorsModification(staticVarCompensators))
            .vscConverterStations(toVscConverterStationsModification(vscConverterStations))
            .shuntCompensators(toShuntCompensatorsModification(shuntCompensators))
            .buses(toBusesModification(buses))
            .build();
    }
}
