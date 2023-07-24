/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.dto.TableEquipmentModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;
import java.util.stream.Collectors;


/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "tableEquipmentModification")
public class TableEquipmentModificationEntity extends ModificationEntity {
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<ModificationEntity> modifications;

    public TableEquipmentModificationEntity(TableEquipmentModificationInfos tableEquipmentModificationInfos) {
        super(tableEquipmentModificationInfos);
        assignAttributes(tableEquipmentModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos tableEquipmentModificationInfos) {
        super.update(tableEquipmentModificationInfos);
        assignAttributes((TableEquipmentModificationInfos) tableEquipmentModificationInfos);
    }

    private List<ModificationEntity> createModificationsEntities(TableEquipmentModificationInfos tableEquipmentModificationInfos) {
        return tableEquipmentModificationInfos.getModifications().stream().map(ModificationInfos::toEntity).collect(Collectors.toList());
    }

    private void assignAttributes(TableEquipmentModificationInfos tableEquipmentModificationInfos) {
        if (modifications == null) {
            modifications = createModificationsEntities(tableEquipmentModificationInfos);
        } else {
            modifications.clear();
            modifications.addAll(createModificationsEntities(tableEquipmentModificationInfos));
        }
    }

    @Override
    public TableEquipmentModificationInfos toModificationInfos() {
        return toTableEquipmentModificationInfosBuilder().build();
    }

    private TableEquipmentModificationInfos.TableEquipmentModificationInfosBuilder<?, ?> toTableEquipmentModificationInfosBuilder() {
        return TableEquipmentModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .modifications(modifications != null ? modifications.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList()) : null);
    }
}
