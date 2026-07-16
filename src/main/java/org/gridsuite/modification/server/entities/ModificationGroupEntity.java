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
import java.util.stream.Collectors;

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

    /**
     * @return a mutable ArrayList of the modifications without those stashed
     */
    public List<ModificationEntity> getActiveModifications() {
        return modifications.stream().filter(m -> !m.getStashed()).collect(Collectors.toCollection(ArrayList::new));
    }

    // adds the modifications to the group and reorders them
    // BUT doesn't remove the previous modifications from the group. This has to be done manually because orphanRemoval is set to false.
    // which means that you may affect a list of the active modifications here, the stashed modifications won't be affected
    public void setModifications(List<ModificationEntity> modifications) {
        this.modifications = modifications;
        modifications.forEach(m -> m.setGroup(this));
        // the unstashed modifications have to be reordered
        List<ModificationEntity> unstashedModifications = getActiveModifications();
        for (int i = 0; i < unstashedModifications.size(); i++) {
            unstashedModifications.get(i).setModificationsOrder(i);
        }
    }
}
