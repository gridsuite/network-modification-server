/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.Country;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.springframework.util.CollectionUtils;
import org.gridsuite.modification.server.dto.*;

import jakarta.persistence.*;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "substationModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "substationModification_id_fk_constraint"))
public class SubstationModificationEntity extends BasicEquipmentModificationEntity {
    @Column(name = "country")
    private Country country;

    @Column(name = "countryOp")
    @Enumerated(EnumType.STRING)
    private OperationType countryOp;

    public SubstationModificationEntity(@NonNull SubstationModificationInfos substationModificationInfos) {
        super(substationModificationInfos);
        assignAttributes(substationModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((SubstationModificationInfos) modificationInfos);
    }

    private void assignAttributes(SubstationModificationInfos substationModificationInfos) {
        this.country = substationModificationInfos.getCountry() != null ? substationModificationInfos.getCountry().getValue() : null;
        this.countryOp = substationModificationInfos.getCountry() != null ? substationModificationInfos.getCountry().getOp() : null;
    }

    @Override
    public SubstationModificationInfos toModificationInfos() {
        return toSubstationModificationInfosBuilder().build();
    }

    private SubstationModificationInfos.SubstationModificationInfosBuilder<?, ?> toSubstationModificationInfosBuilder() {
        return SubstationModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .active(getActive())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .country(AttributeModification.toAttributeModification(getCountry(), getCountryOp()))
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                    getProperties().stream()
                        .map(FreePropertyEntity::toInfos)
                        .toList());
    }
}
