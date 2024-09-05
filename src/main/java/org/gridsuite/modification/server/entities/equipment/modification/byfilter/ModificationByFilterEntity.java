package org.gridsuite.modification.server.entities.equipment.modification.byfilter;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;

import java.util.List;
import java.util.UUID;

import static jakarta.persistence.InheritanceType.JOINED;

@NoArgsConstructor
@Entity
@Inheritance(strategy = JOINED)
@Table(name = "modification_by_filter")
public class ModificationByFilterEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "modification_by_filter_id",
            foreignKey = @ForeignKey(name = "modification_by_filter_id_fk"))
    private List<VariationFilterEntity> filters;

    @Column
    private String editedField;

    protected ModificationByFilterEntity(AbstractModificationByFilterInfos modificationByFilterInfos) {
        this.id = null;
        this.filters = modificationByFilterInfos.getFilters().stream().map(FilterInfos::toEntity).toList();
        this.editedField = modificationByFilterInfos.getEditedField();
    }

    protected void assignAttributes(AbstractModificationByFilterInfos modificationByFilterInfos) {
        modificationByFilterInfos.setId(id);
        modificationByFilterInfos.setFilters(filters.stream()
                .map(filterEntity -> new FilterInfos(filterEntity.getFilterId(), filterEntity.getName()))
                .toList());
        modificationByFilterInfos.setEditedField(editedField);
    }
}
