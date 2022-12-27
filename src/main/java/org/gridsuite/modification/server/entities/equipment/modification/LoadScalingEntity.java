/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.LoadScalingVariation;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.hibernate.Hibernate;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "LoadScaling")
public class LoadScalingEntity extends ModificationEntity {

    @Column(name = "variationType")
    @Enumerated(EnumType.STRING)
    private VariationType variationType;

    @OneToMany(cascade = CascadeType.ALL)
    private List<LoadScalingVariationEntity> variations;

    public LoadScalingEntity(@NonNull LoadScalingInfos loadScalingInfos) {
        super(ModificationType.LOAD_SCALING);
        assignAttributes(loadScalingInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LoadScalingInfos) modificationInfos);
    }

    private void assignAttributes(LoadScalingInfos loadScalingInfos) {
        variationType = loadScalingInfos.getVariationType();
        if (CollectionUtils.isNotEmpty(loadScalingInfos.getLoadScalingVariations())) {
            variations = loadScalingInfos.getLoadScalingVariations().stream()
                    .map(LoadScalingVariation::toEntity)
                    .collect(Collectors.toList());
        }
    }

    @Override
    public LoadScalingInfos toModificationInfos() {
        return toLoadScalingInfosBuilder().build();
    }

    private LoadScalingInfos.LoadScalingInfosBuilder<?, ?> toLoadScalingInfosBuilder() {
        return LoadScalingInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .variationType(getVariationType())
                .loadScalingVariations(getVariations().stream().map(LoadScalingVariationEntity::toLoadScalingVariation).collect(Collectors.toList()));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) {
            return false;
        }
        LoadScalingEntity that = (LoadScalingEntity) o;
        return getId() != null && Objects.equals(getId(), that.getId());
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
