/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.ColumnDefault;
import org.hibernate.annotations.SQLRestriction;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "composite_modification")
public class CompositeModificationEntity extends ModificationEntity implements ModificationContainer {

    @Column(name = "name")
    @ColumnDefault("'My Composite'")
    private String name;

    @OneToMany(cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH})
    @JoinColumn(name = "container_id", foreignKey = @ForeignKey(ConstraintMode.NO_CONSTRAINT), insertable = false, updatable = false)
    @SQLRestriction("container_type = 'COMPOSITE'")
    @OrderBy("modificationsOrder asc")
    @BatchSize(size = 100)
    private List<ModificationEntity> modifications = new ArrayList<>();

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    @Override
    public ModificationContainerType getContainerType() {
        return ModificationContainerType.COMPOSITE;
    }

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream()
                .map(ModificationEntity::toModificationInfos)
                .toList();
        return CompositeModificationInfos.builder()
                .name(getName())
                .activated(getActivated())
                .description(getDescription())
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .modificationsInfos(modificationsInfos)
                .build();
    }

    private void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        this.setName(compositeModificationInfos.getName());
        setModifications(compositeModificationInfos.getModificationsInfos().stream()
            .map(ModificationEntity::fromDTO)
            .toList());
    }

    /**
     * Replace the whole list. Re-numbers {@code modificationsOrder} and re-points each child at
     * this composite.
     */
    public void setModifications(List<ModificationEntity> newChildren) {
        if (newChildren == null) {
            throw new IllegalArgumentException("Modifications list for a composite cannot be null");
        }
        this.modifications.forEach(ModificationEntity::detachFromContainer);
        this.modifications.clear();
        for (int i = 0; i < newChildren.size(); i++) {
            ModificationEntity child = newChildren.get(i);
            child.attachToContainer(this);
            child.setModificationsOrder(i);
            this.modifications.add(child);
        }
    }

    @Override
    public void addModification(ModificationEntity child, int position) {
        ContainerOps.insert(this, this.modifications, child, position);
    }
}
