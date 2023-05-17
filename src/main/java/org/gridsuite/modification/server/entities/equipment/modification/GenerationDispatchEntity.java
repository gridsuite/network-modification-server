/*
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.GenerationDispatchInfos;
import org.gridsuite.modification.server.dto.GeneratorsWithoutOutageInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "generationDispatch")
public class GenerationDispatchEntity extends ModificationEntity {

    @Column(name = "lossCoefficient")
    private double lossCoefficient;

    @Column(name = "defaultOutageRate")
    private double defaultOutageRate;

    @ElementCollection
    @CollectionTable(name = "generatorsWithoutOutage")
    private List<GeneratorsWithoutOutageEmbeddable> generatorsWithoutOutage;

    public GenerationDispatchEntity(@NotNull GenerationDispatchInfos generationDispatchInfos) {
        super(generationDispatchInfos);
        assignAttributes(generationDispatchInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((GenerationDispatchInfos) modificationInfos);
    }

    private void assignAttributes(GenerationDispatchInfos generationDispatchInfos) {
        lossCoefficient = generationDispatchInfos.getLossCoefficient();
        defaultOutageRate = generationDispatchInfos.getDefaultOutageRate();
        generatorsWithoutOutage = toEmbeddable(generationDispatchInfos.getGeneratorsWithoutOutage());
    }

    public static List<GeneratorsWithoutOutageEmbeddable> toEmbeddable(List<GeneratorsWithoutOutageInfos> generators) {
        return generators == null ? null : generators.stream()
            .map(generator -> new GeneratorsWithoutOutageEmbeddable(generator.getId(), generator.getName()))
            .collect(Collectors.toList());
    }

    @Override
    public GenerationDispatchInfos toModificationInfos() {
        List<GeneratorsWithoutOutageInfos> generators = generatorsWithoutOutage != null ? generatorsWithoutOutage
                .stream()
                .map(generator -> new GeneratorsWithoutOutageInfos(generator.getId(), generator.getName()))
                .collect(Collectors.toList()) : null;

        return GenerationDispatchInfos.builder()
                .date(getDate())
                .uuid(getId())
                .lossCoefficient(getLossCoefficient())
                .defaultOutageRate(getDefaultOutageRate())
                .generatorsWithoutOutage(generators)
                .build();
    }
}
