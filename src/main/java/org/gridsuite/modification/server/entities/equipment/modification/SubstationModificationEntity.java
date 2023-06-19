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

import javax.persistence.*;
import java.util.List;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "substationModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "substationModification_id_fk_constraint"))
public class SubstationModificationEntity extends BasicEquipmentModificationEntity {
    @Column(name = "substationCountryValue")
    private Country substationCountryValue;

    @Column(name = "substationCountryOp")
    @Enumerated(EnumType.STRING)
    private OperationType substationCountryOp;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "substation_modification_id")
    @OrderColumn(name = "insert_position")
    List<SubstationFreePropertyEntity> properties;

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
        this.substationCountryValue = substationModificationInfos.getSubstationCountry() != null ? substationModificationInfos.getSubstationCountry().getValue() : null;
        this.substationCountryOp = substationModificationInfos.getSubstationCountry() != null ? substationModificationInfos.getSubstationCountry().getOp() : null;
        List<SubstationFreePropertyEntity> newProperties = substationModificationInfos.getProperties() == null ? null :
                substationModificationInfos.getProperties().stream()
                        .map(prop -> SubstationFreePropertyEntity.builder()
                                .name(prop.getName())
                                .value(prop.getValue())
                                .deletionMark(prop.isDeletionMark())
                                .added(prop.isAdded())
                                .build())
                        .collect(Collectors.toList());
        if (this.properties != null) {
            // update using the same reference with clear/add (to avoid JPA exception)
            this.properties.clear();
            if (newProperties != null) {
                this.properties.addAll(newProperties);
            }
        } else {
            this.properties = newProperties;
        }
    }

    @Override
    public SubstationModificationInfos toModificationInfos() {
        return toSubstationModificationInfosBuilder().build();
    }

    private SubstationModificationInfos.SubstationModificationInfosBuilder<?, ?> toSubstationModificationInfosBuilder() {
        return SubstationModificationInfos
                .builder()
                .uuid(getId())
                .groupUuid(getGroup().getId())
                .date(getDate())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .substationCountry(AttributeModification.toAttributeModification(getSubstationCountryValue(), getSubstationCountryOp()))
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(prop -> SubstationFreePropertyInfos.builder()
                                        .name(prop.getName())
                                        .value(prop.getValue())
                                        .deletionMark(prop.getDeletionMark())
                                        .added(prop.getAdded())
                                        .build())
                                .collect(Collectors.toList())
                        );
    }
}
