/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.hibernate.annotations.ColumnDefault;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "composite_modification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "composite_modification_id_fk_constraint"))
public class CompositeModificationEntity extends ModificationEntity implements ModificationContainer {

    @Column(name = "name")
    @ColumnDefault("'My Composite'")
    private String name;

    @OneToOne(cascade = CascadeType.ALL, optional = false, fetch = FetchType.LAZY)
    @PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "composite_modification_content_fk"))
    private CompositeContainerEntity content;

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = getModifications().stream()
                .map(ModificationEntity::toModificationInfos)
                .toList();
        return CompositeModificationInfos.builder()
                .name(getName())
                .description(getDescription())
                .date(getDate())
                .activated(getActivated())
                .uuid(getId())
                .stashed(getStashed())
                .modificationsInfos(modificationsInfos)
                .build();
    }

    protected void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        this.name = compositeModificationInfos.getName();
        this.content = new CompositeContainerEntity(getId());
        setModifications(compositeModificationInfos.getModificationsInfos().stream()
                .map(ModificationEntity::fromDTO)
                .toList());
    }

    @Override
    public List<ModificationEntity> getModifications() {
        return content.getModifications();
    }

    @Override
    public void setModifications(List<ModificationEntity> newChildren) {
        content.setModifications(newChildren);
    }

    @Override
    public void addModification(ModificationEntity child, int position) {
        content.addModification(child, position);
    }
}
