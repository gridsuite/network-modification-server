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
import org.gridsuite.modification.server.dto.MassiveEquipmentsModificationsInfos;
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
@Table(name = "MassiveEquipmentsModifications")
public class MassiveEquipmentsModificationsEntity extends ModificationEntity {
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<ModificationEntity> modifications;

    public MassiveEquipmentsModificationsEntity(MassiveEquipmentsModificationsInfos massiveEquipmentsModificationsInfos) {
        super(massiveEquipmentsModificationsInfos);
        assignAttributes(massiveEquipmentsModificationsInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos massiveEquipmentsModificationsInfos) {
        super.update(massiveEquipmentsModificationsInfos);
        assignAttributes((MassiveEquipmentsModificationsInfos) massiveEquipmentsModificationsInfos);
    }

    private void assignAttributes(MassiveEquipmentsModificationsInfos massiveEquipmentsModificationsInfos) {
        if (modifications == null) {
            modifications = massiveEquipmentsModificationsInfos.getModifications().stream().map(ModificationInfos::toEntity).collect(Collectors.toList());
        } else {
            modifications.clear();
            modifications.addAll(massiveEquipmentsModificationsInfos.getModifications().stream().map(ModificationInfos::toEntity).collect(Collectors.toList()));
        }
    }

    @Override
    public MassiveEquipmentsModificationsInfos toModificationInfos() {
        return toMassiveEquipmentsModificationInfosBuilder().build();
    }

    private MassiveEquipmentsModificationsInfos.MassiveEquipmentsModificationsInfosBuilder<?, ?> toMassiveEquipmentsModificationInfosBuilder() {
        return MassiveEquipmentsModificationsInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .modifications(modifications != null ? modifications.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList()) : null);
    }
}
