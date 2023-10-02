package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

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
        modifications = tabularModificationInfos.getModifications;
    }
}
