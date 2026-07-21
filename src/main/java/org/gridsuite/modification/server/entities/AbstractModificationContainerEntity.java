/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.NetworkModificationException;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;

/**
 * @author Hugo Marcellin {@literal <hugo.marcelin at rte-france.com>}
 */
@Getter
@NoArgsConstructor
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "modification_container")
public abstract class AbstractModificationContainerEntity extends AbstractManuallyAssignedIdentifierEntity<UUID> {

    @Id
    @Column(name = "id")
    private UUID id;

    @Column(name = "type")
    private String type;

    @OneToMany(mappedBy = "container", cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH})
    @OrderBy("modificationsOrder asc")
    private final List<ModificationEntity> modifications = new ArrayList<>();

    protected AbstractModificationContainerEntity(UUID id, ModificationContainerType type) {
        this.id = id;
        this.type = type.name();
    }

    public boolean containsOrIsContainer(UUID targetId) {
        return getId().equals(targetId)
                || getModifications().stream().anyMatch(sub -> isOrContains(sub, targetId));
    }

    private boolean isOrContains(ModificationEntity sub, UUID targetId) {
        if (sub.getId().equals(targetId)) {
            return true;
        }
        return sub instanceof CompositeModificationEntity composite
                && composite.getContent().containsOrIsContainer(targetId);
    }

    /** Active (non-stashed, non-null) modifications as a mutable working copy, in current order */
    public List<ModificationEntity> getActiveModifications() {
        return modifications.stream()
                .filter(Objects::nonNull)
                .filter(m -> !Boolean.TRUE.equals(m.getStashed()))
                .collect(Collectors.toCollection(ArrayList::new));
    }

    private int indexOf(List<ModificationEntity> view, UUID referenceUuid) {
        for (int i = 0; i < view.size(); i++) {
            if (referenceUuid.equals(view.get(i).getId())) {
                return i;
            }
        }
        throw new NetworkModificationException(MODIFICATION_NOT_FOUND,
                String.format("Modification %s not found in target container %s", referenceUuid, getId()));
    }

    public void renumberActiveModificationsOnly() {
        renumber(getActiveModifications());
    }

    private void renumber(List<ModificationEntity> orderedActives) {
        for (int i = 0; i < orderedActives.size(); i++) {
            orderedActives.get(i).setContainer(this);
            orderedActives.get(i).setModificationsOrder(i);
        }
    }

    private List<ModificationEntity> removeFrom(List<ModificationEntity> view, List<UUID> orderedIdsToRemove) {
        Set<UUID> idsToRemove = new HashSet<>(orderedIdsToRemove);
        Map<UUID, ModificationEntity> removedById = new HashMap<>();
        view.removeIf(e -> idsToRemove.contains(e.getId()) && removedById.put(e.getId(), e) == null);

        return orderedIdsToRemove.stream()
                .map(removedById::get)
                .filter(Objects::nonNull)
                .toList();
    }

    public List<ModificationEntity> extractModifications(List<UUID> uuidsToExtract) {
        List<ModificationEntity> actives = getActiveModifications();
        List<ModificationEntity> extracted = removeFrom(actives, uuidsToExtract);
        modifications.removeAll(extracted);
        return extracted;
    }

    public List<ModificationEntity> moveModificationsWithin(List<UUID> uuidsToMove, UUID beforeModificationUuid) {
        List<ModificationEntity> actives = getActiveModifications();
        List<ModificationEntity> moved = removeFrom(actives, uuidsToMove);
        if (moved.isEmpty()) {
            return List.of();
        }
        int insertionIndex = beforeModificationUuid == null ? actives.size() : indexOf(actives, beforeModificationUuid);
        actives.addAll(insertionIndex, moved);
        renumber(actives);
        return moved;
    }

    public void addModification(ModificationEntity child, int modificationsOrder) {
        child.setContainer(this);
        child.setModificationsOrder(modificationsOrder);
        modifications.add(child);
    }

    public void insertModifications(List<ModificationEntity> toInsert, UUID beforeModificationUuid) {
        List<ModificationEntity> modifs = new ArrayList<>(modifications);
        int insertionIndex = beforeModificationUuid == null ? modifs.size() : indexOf(modifs, beforeModificationUuid);
        modifs.addAll(insertionIndex, toInsert);
        setModifications(modifs);
    }

    public void setModifications(List<ModificationEntity> newChildren) {
        List<ModificationEntity> incoming = newChildren == null ? List.of() : new ArrayList<>(newChildren);
        this.modifications.clear();
        for (ModificationEntity child : incoming) {
            child.setContainer(this);
            this.modifications.add(child);
        }
        renumberActiveModificationsOnly();
    }
}
