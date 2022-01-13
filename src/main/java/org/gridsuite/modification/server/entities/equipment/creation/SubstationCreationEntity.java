/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.Country;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.SubstationCreationInfos;

import javax.persistence.*;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "substationCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "substationCreation_id_fk_constraint"))
public class SubstationCreationEntity extends EquipmentCreationEntity {

    @Column(name = "country")
    private Country country;

    public SubstationCreationEntity(String equipmentId, String equipmentName, Country country) {
        super(ModificationType.SUBSTATION_CREATION,
                equipmentId,
                equipmentName);
        this.country = country;
    }

    @Override
    public SubstationCreationInfos toModificationInfos() {
        return toSubstationCreationInfosBuilder().build();
    }

    public SubstationCreationInfos toSubstationCreationInfos() {
        return toSubstationCreationInfosBuilder().build();
    }

    private SubstationCreationInfos.SubstationCreationInfosBuilder<?, ?> toSubstationCreationInfosBuilder() {
        return SubstationCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .active(isActive())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .substationCountry(getCountry());
    }
}

