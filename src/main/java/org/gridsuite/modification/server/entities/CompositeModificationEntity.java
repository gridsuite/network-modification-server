/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.OneToOne;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Getter
@Setter
@Entity
@Table(name = "composite_modification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "composite_modification_id_fk_constraint"))
public class CompositeModificationEntity extends ModificationEntity {

    @OneToOne(cascade = {CascadeType.PERSIST, CascadeType.MERGE}, optional = false)
    @PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "composite_modification_content_fk"))
    private CompositeContainerEntity content;

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    public CompositeModificationEntity() {}

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = getModifications().stream()
                .map(ModificationEntity::toModificationInfos)
                .toList();
        return CompositeModificationInfos.builder()
                .name(getName())
                .activated(getActivated())
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .modificationsInfos(modificationsInfos)
                .build();
    }

    protected void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        // The content takes that SAME id — the shared-PK guarantee, established at construction.
        this.content = new CompositeContainerEntity(getId(), compositeModificationInfos.getName());
        setModifications(compositeModificationInfos.getModificationsInfos().stream()
                .map(ModificationEntity::fromDTO)
                .toList());
    }

    public String getName() {
        return content == null ? null : content.getName();
    }

    public void setName(String name) {
        ensureContent().setName(name);
    }

    public List<ModificationEntity> getModifications() {
        return content == null ? List.of() : content.getModifications();
    }

    public void setModifications(List<ModificationEntity> newChildren) {
        ensureContent().setModifications(newChildren);
    }

    private CompositeContainerEntity ensureContent() {
        if (content == null) {
            // getId() must be assigned by now (it is, for a persisted/constructed leaf)
            content = new CompositeContainerEntity(getId(), null);
        }
        return content;
    }
}
