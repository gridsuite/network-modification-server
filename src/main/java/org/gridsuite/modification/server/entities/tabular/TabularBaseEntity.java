/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.tabular;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.TabularBaseInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class TabularBaseEntity extends ModificationEntity {

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "tabular_modification_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(name = "tabular_modification_id_fk_constraint")
    )
    @OrderColumn(name = "insert_position")
    private List<TabularPropertyEntity> properties;

    private String csvFilename;

    protected TabularBaseEntity(TabularBaseInfos tabularBaseInfos) {
        super(tabularBaseInfos);
        assignAttributes(tabularBaseInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((TabularBaseInfos) modificationInfos);
    }

    private void assignAttributes(TabularBaseInfos tabularBaseInfos) {
        List<TabularPropertyEntity> newProperties = tabularBaseInfos.getProperties() == null ? null :
                tabularBaseInfos.getProperties().stream()
                        .map(TabularPropertyEntity::new)
                        .toList();
        if (this.properties != null) {
            this.properties.clear();
            if (newProperties != null) {
                this.properties.addAll(newProperties);
            }
        } else {
            this.properties = newProperties;
        }
        this.csvFilename = tabularBaseInfos.getCsvFilename();
    }
}
