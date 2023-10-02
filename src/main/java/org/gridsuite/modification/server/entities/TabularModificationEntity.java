package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.gridsuite.modification.server.dto.VoltageInitGeneratorModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VoltageInitGeneratorModificationEmbeddable;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "tabular_modification")
public class TabularModificationEntity extends ModificationEntity {

    @OneToMany(
            mappedBy = "tabularModification",
            cascade = CascadeType.ALL
    )
    @OrderColumn
    private List<ModificationEntity> modifications = new ArrayList<>();

    public TabularModificationEntity(TabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        modifications = toEmbeddableVoltageInitGenerators(tabularModificationInfos.getGenerators());
    }

    public static List<VoltageInitGeneratorModificationEmbeddable> toEmbeddableVoltageInitGenerators(List<ModificationInfos> generators) {
        return generators == null ? null : generators.stream()
                .map(generator -> new VoltageInitGeneratorModificationEmbeddable(generator.getGeneratorId(), generator.getVoltageSetpoint(), generator.getReactivePowerSetpoint()))
                .collect(Collectors.toList());
    }
}
