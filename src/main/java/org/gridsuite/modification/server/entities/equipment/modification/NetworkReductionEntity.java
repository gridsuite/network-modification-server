/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.NetworkReductionInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "network_reduction")
public class NetworkReductionEntity extends ModificationEntity {

    @ElementCollection
    @CollectionTable(name = "network_reduction_excluded_voltage_levels")
    private List<GeneratorsFilterEmbeddable> excludedVoltageLevels;

    public NetworkReductionEntity(@NonNull NetworkReductionInfos networkReductionInfos) {
        super(networkReductionInfos);
        assignAttributes(networkReductionInfos);
    }

    @Override
    public NetworkReductionInfos toModificationInfos() {
        return NetworkReductionInfos.builder()
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .activated(getActivated())
                .description(getDescription())
                .filters(toFilters(excludedVoltageLevels))
                .build();
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((NetworkReductionInfos) modificationInfos);
    }

    private void assignAttributes(NetworkReductionInfos networkReductionInfos) {
        excludedVoltageLevels = networkReductionInfos.getFilters() == null ? null : networkReductionInfos.getFilters().stream()
                .map(generator -> new GeneratorsFilterEmbeddable(generator.getId(), generator.getName()))
                .collect(Collectors.toList());
    }

    private List<FilterInfos> toFilters(List<GeneratorsFilterEmbeddable> filters) {
        return filters != null ? filters
                .stream()
                .map(f -> new FilterInfos(f.getId(), f.getName()))
                .collect(Collectors.toList()) : null;
    }
}
