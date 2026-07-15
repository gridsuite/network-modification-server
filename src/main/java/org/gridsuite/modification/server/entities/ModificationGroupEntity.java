/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "modificationGroup")
public class ModificationGroupEntity extends AbstractManuallyAssignedIdentifierEntity<UUID> {
    @Id
    @Column(name = "id")
    private UUID id;

    @OneToMany(
            mappedBy = "group",
            //Remove is not here because we handle the deletion manually
            cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH}
    )
    @OrderBy("modificationsOrder asc")
    private List<ModificationEntity> modifications = new ArrayList<>();

    public ModificationGroupEntity(UUID uuid) {
        this.id = uuid;
    }

    public void addModification(ModificationEntity modification) {
        if (modifications.isEmpty()) {
            // when we go back to an empty list, dont use add() on the list because JPA could start @OrderColumn to 1 instead of 0
            List<ModificationEntity> newList = new ArrayList<>();
            newList.add(modification);
            setModifications(newList);
        } else {
            modifications.add(modification);
            modification.setGroup(this);
        }
    }

    public void setModifications(List<ModificationEntity> modifications) {
        this.modifications = modifications;
        modifications.forEach(m -> m.setGroup(this));
        // the unstashed modifications have to be reordered
        List<ModificationEntity> unstashedModifications = modifications.stream()
                .filter(m -> !m.getStashed()).toList();
        for (int i = 0; i < unstashedModifications.size(); i++) {
            unstashedModifications.get(i).setModificationsOrder(i);
        }
    }
}
