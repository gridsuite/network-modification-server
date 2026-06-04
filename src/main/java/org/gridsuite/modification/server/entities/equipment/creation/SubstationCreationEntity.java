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

import jakarta.persistence.*;
import org.gridsuite.modification.dto.ModificationDto;
import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.model.SubstationCreationModel;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

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

    private void assignAttributes(SubstationCreationModel substationCreationInfos) {
        country = substationCreationInfos.getCountry();
    }

    public SubstationCreationEntity(SubstationCreationInfos substationCreationInfos) {
        super((ModificationDto) substationCreationInfos);
        assignAttributes(substationCreationInfos);
    }

    public SubstationCreationEntity(SubstationCreationModel substationCreationInfos) {
        super(substationCreationInfos);
        assignAttributes(substationCreationInfos);
    }

    @Override
    public void update(ModificationDto modificationInfos) {
        super.update(modificationInfos);
        SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) modificationInfos;
        assignAttributes(substationCreationInfos);
    }

    @Override
    public SubstationCreationInfos toModificationInfos() {
        return toSubstationCreationInfosBuilder().build();
    }

    public SubstationCreationModel toSubstationCreationModel() {
        return SubstationCreationModel.builder().equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .country(getCountry())
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                getProperties().stream()
                    .map(FreePropertyEntity::toModel)
                    .toList()).build();
    }

    private SubstationCreationInfos.SubstationCreationInfosBuilder<?, ?> toSubstationCreationInfosBuilder() {
        return SubstationCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .description(getDescription())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .country(getCountry())
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                getProperties().stream()
                    .map(FreePropertyEntity::toModel)
                    .toList());
    }
}

