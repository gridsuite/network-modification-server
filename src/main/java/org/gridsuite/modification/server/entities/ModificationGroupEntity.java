/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.SQLRestriction;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "modificationGroup")
public class ModificationGroupEntity extends AbstractManuallyAssignedIdentifierEntity<UUID> implements ModificationContainer {
    @Id
    @Column(name = "id")
    private UUID id;

    /**
     * Children of this group. The {@code @JoinColumn} writes {@code container_id}; the
     * {@code @SQLRestriction} ensures we only read rows whose {@code container_type = 'GROUP'}.
     * The {@code container_type} value itself is set on the child via
     * {@link ModificationEntity#attachToContainer(ModificationContainer)} in {@link #addModification}.
     *
     * <p>Deletion is handled manually (see NetworkModificationRepository), so no remove/orphan cascade here.
     */
    @OneToMany(cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH})
    @JoinColumn(name = "container_id", foreignKey = @ForeignKey(ConstraintMode.NO_CONSTRAINT))
    @SQLRestriction("container_type = 'GROUP'")
    @OrderBy("modificationsOrder asc")
    private List<ModificationEntity> modifications = new ArrayList<>();

    public ModificationGroupEntity(UUID uuid) {
        this.id = uuid;
    }

    @Override
    public ModificationContainerType getContainerType() {
        return ModificationContainerType.GROUP;
    }

    @Override
    public void addModification(ModificationEntity child, int position) {
        ContainerOps.insert(this, this.modifications, child, position);
    }

    @Override
    public boolean removeModification(UUID childId) {
        return ContainerOps.removeById(this.modifications, childId);
    }

    /**
     * Replace the whole list (used during initial load / re-ordering operations).
     * Re-numbers {@code modificationsOrder} and re-points each child at this group.
     */
    public void setModifications(List<ModificationEntity> newChildren) {
        this.modifications.clear();
        if (newChildren == null || newChildren.isEmpty()) {
            return;
        }
        for (int i = 0; i < newChildren.size(); i++) {
            ModificationEntity child = newChildren.get(i);
            child.attachToContainer(this);
            child.setModificationsOrder(i);
            this.modifications.add(child);
        }
    }
}
