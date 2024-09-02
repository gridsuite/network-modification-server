/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.BySimpleModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationByFilterInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.SimpleModificationEntity;

import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "bySimpleModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "bySimpleModification_id_fk_constraint"))
public class BySimpleModificationEntity extends ModificationEntity {
    @Enumerated(EnumType.STRING)
    @Column
    private IdentifiableType equipmentType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "by_simple_modification_id",
            foreignKey = @ForeignKey(name = "by_simple_modification_id_fk"))
    private List<SimpleModificationEntity> simpleModificationEntities;

    public BySimpleModificationEntity(BySimpleModificationInfos bySimpleModificationInfos) {
        super(bySimpleModificationInfos);
        assignAttributes(bySimpleModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BySimpleModificationInfos) modificationInfos);
    }

    private void assignAttributes(BySimpleModificationInfos bySimpleModificationInfos) {
        this.equipmentType = bySimpleModificationInfos.getEquipmentType();
        if (simpleModificationEntities == null) {
            simpleModificationEntities = bySimpleModificationInfos.getSimpleModificationInfosList()
                    .stream()
                    .map(AbstractSimpleModificationByFilterInfos::toEntity)
                    .toList();
        } else {
            simpleModificationEntities.clear();
            simpleModificationEntities.addAll(bySimpleModificationInfos.getSimpleModificationInfosList()
                    .stream()
                    .map(AbstractSimpleModificationByFilterInfos::toEntity)
                    .toList());
        }
    }

    @Override
    public BySimpleModificationInfos toModificationInfos() {
        return toBySimpleModificationInfosBuilder().build();
    }

    private BySimpleModificationInfos.BySimpleModificationInfosBuilder<?, ?> toBySimpleModificationInfosBuilder() {
        return BySimpleModificationInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .equipmentType(equipmentType)
            .simpleModificationInfosList(simpleModificationEntities.stream()
                .map(SimpleModificationEntity::toSimpleModificationInfos)
                .toList()
            );
    }
}
