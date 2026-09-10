/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.*;
import lombok.*;
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
public class CompositeModificationEntity extends ModificationEntity {

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
                .map(CompositeModificationEntity::fillDisplayMessage)
                .toList();
        return toModificationInfosBuilder(CompositeModificationInfos.builder()
                .name(getName())
                .modificationsInfos(modificationsInfos))
                .build();
    }

    @SneakyThrows
    public static ModificationInfos fillDisplayMessage(ModificationInfos child) {
        child.setMessageType(child.getType().name());
        child.setMessageValues(new ObjectMapper().writeValueAsString(child.getMapMessageValues()));
        return child;
    }

    protected void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        this.name = compositeModificationInfos.getName();
        this.content = new CompositeContainerEntity(getId());
        setModifications(compositeModificationInfos.getModificationsInfos().stream()
                .map(ModificationEntity::fromDTO)
                .toList());
    }

    public List<ModificationEntity> getModifications() {
        return content.getModifications();
    }

    public void setModifications(List<ModificationEntity> newChildren) {
        content.setModifications(newChildren);
    }

    public void addModification(ModificationEntity child, int position) {
        content.addModification(child, position);
    }
}
