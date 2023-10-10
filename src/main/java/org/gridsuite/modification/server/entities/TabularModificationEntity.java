package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
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

    @OneToMany(
            mappedBy = "tabularModification",
            cascade = CascadeType.ALL
    )
    @OrderColumn
    private List<ModificationEntity> modifications = new ArrayList<>();

    public TabularModificationEntity(TabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        modificationType = tabularModificationInfos.getModificationType();
        switch (modificationType) {
            case "GENERATOR_MODIFICATION":
                modifications = tabularModificationInfos.getModifications().stream().map(generatorModificationInfos -> new GeneratorModificationEntity((GeneratorModificationInfos) generatorModificationInfos, this)).collect(Collectors.toList());
                break;
            case "LOAD_MODIFICATION":
                modifications = tabularModificationInfos.getModifications().stream().map(loadModificationInfos -> new LoadModificationEntity((LoadModificationInfos) loadModificationInfos, this)).collect(Collectors.toList());
                break;
            default:
                break;
        }
    }

    @Override
    public TabularModificationInfos toModificationInfos() {
        List<EquipmentModificationInfos> modificationsInfos = new ArrayList<>();
        switch (modificationType) {
            case "GENERATOR_MODIFICATION":
                modificationsInfos = modifications.stream().map(generatorModificationEntity -> ((GeneratorModificationEntity) generatorModificationEntity).toModificationInfos()).collect(Collectors.toList());
                break;
            case "LOAD_MODIFICATION":
                modificationsInfos = modifications.stream().map(loadModificationEntity -> ((LoadModificationEntity) loadModificationEntity).toModificationInfos()).collect(Collectors.toList());
                break;
            default:
                break;
        }

        return TabularModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .modificationType(modificationType)
                .modifications(modificationsInfos)
                .build();
    }
}
