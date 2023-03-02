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
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.SubstationCreationInfos;

import javax.persistence.*;
import java.util.HashMap;
import java.util.Map;

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

    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable
    private Map<String, String> properties;

    private void assignAttributes(SubstationCreationInfos substationCreationInfos) {
        country = substationCreationInfos.getSubstationCountry();
        properties = substationCreationInfos.getProperties() == null ? null : new HashMap<>(substationCreationInfos.getProperties());
    }

    public SubstationCreationEntity(SubstationCreationInfos substationCreationInfos) {
        super(substationCreationInfos);
        assignAttributes(substationCreationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) modificationInfos;
        assignAttributes(substationCreationInfos);
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
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .substationCountry(getCountry())
                .properties(getProperties() == null || getProperties().size() == 0 ? null : getProperties());
    }
}

