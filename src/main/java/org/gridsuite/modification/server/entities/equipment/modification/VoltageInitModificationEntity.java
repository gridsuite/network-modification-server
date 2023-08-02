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
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Index;
import javax.persistence.Table;
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
    }

    public static List<VoltageInitGeneratorModificationEmbeddable> toEmbeddableVoltageInitGenerators(List<VoltageInitGeneratorModificationInfos> generators) {
        return generators == null ? null : generators.stream()
            .map(generator -> new VoltageInitGeneratorModificationEmbeddable(generator.getGeneratorId(), generator.getVoltageSetpoint(), generator.getReactivePowerSetpoint()))
            .collect(Collectors.toList());
    }

    private List<VoltageInitGeneratorModificationInfos> toGeneratorsModification(List<VoltageInitGeneratorModificationEmbeddable> generators) {
        return generators != null ? generators
            .stream()
            .map(generator -> new VoltageInitGeneratorModificationInfos(generator.getGeneratorId(), generator.getVoltageSetpoint(), generator.getReactivePowerSetpoint()))
            .collect(Collectors.toList()) : null;
    }

    @Override
    public VoltageInitModificationInfos toModificationInfos() {
        return VoltageInitModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .generators(toGeneratorsModification(generators))
                .build();
    }
}
