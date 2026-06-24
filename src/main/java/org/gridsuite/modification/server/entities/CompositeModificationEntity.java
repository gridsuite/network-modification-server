/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.hibernate.annotations.ColumnDefault;

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
public class CompositeModificationEntity extends ModificationEntity {

    @Column(name = "name")
    @ColumnDefault("'My Composite'")
    private String name;

    @OneToMany(cascade = CascadeType.ALL)
    @JoinTable(
            name = "compositeModificationSubModifications",
            joinColumns = @JoinColumn(name = "id"), foreignKey = @ForeignKey(name = "composite_modification_sub_modifications_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "modificationId"), inverseForeignKey = @ForeignKey(name = "modification_id_fk"))
    @OrderBy("modificationsOrder asc")
    private List<ModificationEntity> modifications = new ArrayList<>();

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream()
                .map(ModificationEntity::toModificationInfos)
                .peek(this::fillDisplayMessage)
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

    private void fillDisplayMessage(ModificationInfos child) {
        if (child.getMessageType() == null) {
            child.setMessageType(child.getType().name());
        }
        if (child.getMessageValues() == null) {
            try {
                child.setMessageValues(new ObjectMapper().writeValueAsString(child.getMapMessageValues()));
            } catch (JsonProcessingException e) {
                child.setMessageValues("{}");
            }
        }
    }

    private void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        this.setName(compositeModificationInfos.getName());
        setModifications(compositeModificationInfos.getModificationsInfos().stream()
            .map(ModificationEntity::fromDTO)
            .toList());
    }

    public void setModifications(List<ModificationEntity> modifications) {
        if (modifications == null) {
            throw new IllegalArgumentException("Modifications list for a composite cannot be null");
        }
        this.modifications.clear();
        this.modifications.addAll(modifications);
        for (int i = 0; i < this.modifications.size(); i++) {
            this.modifications.get(i).setModificationsOrder(i);
        }
    }
}
