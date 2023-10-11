package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;
import java.util.stream.Collectors;

import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "tabular_modification")
public class TabularModificationEntity extends ModificationEntity {

    @Column(name = "modificationType")
    private String modificationType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn
    private List<ModificationEntity> modifications;

    public TabularModificationEntity(TabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        modificationType = tabularModificationInfos.getModificationType();
        switch (modificationType) {
            case "GENERATOR_MODIFICATION":
                modifications = tabularModificationInfos.getModifications().stream().map(generatorModificationInfos -> new GeneratorModificationEntity((GeneratorModificationInfos) generatorModificationInfos)).collect(Collectors.toList());
                break;
            case "LOAD_MODIFICATION":
                modifications = tabularModificationInfos.getModifications().stream().map(loadModificationInfos -> new LoadModificationEntity((LoadModificationInfos) loadModificationInfos)).collect(Collectors.toList());
                break;
            default:
                break;
        }
    }

    @Override
    public TabularModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList());
        return TabularModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .modificationType(modificationType)
                .modifications(modificationsInfos)
                .build();
    }
}
