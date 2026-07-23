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
import java.util.function.Function;
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

    @OneToMany(
        mappedBy = "container",
        // Remove is not here because we handle the deletion manually
        cascade = {CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH})
    @OrderBy("modificationsOrder asc")
    private final List<ModificationEntity> modifications = new ArrayList<>();

    protected AbstractModificationContainerEntity(UUID id, ModificationContainerType type) {
        this.id = id;
        this.type = type.name();
    }

    public boolean isGroup() {
        return ModificationContainerType.GROUP.name().equals(type);
    }

    public boolean isComposite() {
        return ModificationContainerType.COMPOSITE.name().equals(type);
    }

    public List<ModificationEntity> getNonStashedModifications() {
        return modifications.stream()
                .filter(Objects::nonNull)
                .filter(m -> !Boolean.TRUE.equals(m.getStashed()))
                .collect(Collectors.toCollection(ArrayList::new));
    }

    private int indexOf(UUID referenceUuid) {
        List<ModificationEntity> modifications = getNonStashedModifications();
        for (int i = 0; i < modifications.size(); i++) {
            if (referenceUuid.equals(modifications.get(i).getId())) {
                return i;
            }
        }
        throw new NetworkModificationException(MODIFICATION_NOT_FOUND,
            String.format("Modification %s not found in target container %s", referenceUuid, getId()));
    }

    private int getNextPositionOrder() {
        return getNonStashedModifications().stream()
            .mapToInt(ModificationEntity::getModificationsOrder)
            .max()
            .orElse(0) + 1;
    }

    private int getPositionOrder(UUID beforeModificationUuid) {
        return getNonStashedModifications().stream()
            .filter(m -> m.getId().equals(beforeModificationUuid))
            .mapToInt(ModificationEntity::getModificationsOrder)
            .findFirst()
            .getAsInt();
    }

    private void resetPositionsOrder() {
        List<ModificationEntity> modifications = getNonStashedModifications();
        for (int i = 0; i < modifications.size(); i++) {
            modifications.get(i).setModificationsOrder(i);
        }
    }

    private List<ModificationEntity> getOrderedModifications(List<UUID> orderedIdsToRemove) {
        Set<UUID> idsSetToRemove = new HashSet<>(orderedIdsToRemove);
        Map<UUID, ModificationEntity> modificationsToBeRemoved = getNonStashedModifications().stream()
            .filter(e -> e != null && idsSetToRemove.contains(e.getId()))
            .collect(Collectors.toMap(ModificationEntity::getId, Function.identity()));

        return orderedIdsToRemove.stream()
            .map(modificationsToBeRemoved::get)
            .filter(Objects::nonNull)
            .toList();
    }

    public List<ModificationEntity> removeModifications(List<UUID> uuidsToRemove) {
        List<ModificationEntity> modificationsToBeRemoved = getOrderedModifications(uuidsToRemove);
        modifications.removeAll(modificationsToBeRemoved);
        return modificationsToBeRemoved;
    }

    public List<ModificationEntity> moveModifications(List<UUID> uuidsToMove, UUID beforeModificationUuid) {
        List<ModificationEntity> modificationsToBeMoved = removeModifications(uuidsToMove);
        if (modificationsToBeMoved.isEmpty()) {
            return List.of();
        }
        insertModifications(modificationsToBeMoved, beforeModificationUuid);
        return modificationsToBeMoved;
    }

    public void addModification(ModificationEntity child, int modificationsOrder) {
        child.setContainer(this);
        child.setModificationsOrder(modificationsOrder);
        modifications.add(child);
    }

    public void insertModifications(List<ModificationEntity> toInsert, UUID beforeModificationUuid) {
        int insertionIndex = beforeModificationUuid == null ? modifications.size() : indexOf(beforeModificationUuid);
        List<ModificationEntity> modifications = getNonStashedModifications();
        modifications.addAll(insertionIndex, toInsert);
        setModifications(modifications);
    }

    public void setModifications(List<ModificationEntity> newChildren) {
        List<ModificationEntity> incoming = newChildren == null ? List.of() : new ArrayList<>(newChildren);
        this.modifications.clear();
        for (ModificationEntity child : incoming) {
            child.setContainer(this);
            this.modifications.add(child);
        }
        resetPositionsOrder();
    }
}
