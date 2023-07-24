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
import org.gridsuite.modification.server.dto.GeneratorsFilterInfos;
import org.gridsuite.modification.server.dto.GeneratorsFrequencyReserveInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.SubstationsGeneratorsOrderingInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.util.CollectionUtils;

import javax.persistence.CascadeType;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OrderColumn;
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
    private List<GeneratorsFilterEmbeddable> generatorsWithoutOutage;

    @ElementCollection
    @CollectionTable(name = "generatorsWithFixedSupply")
    private List<GeneratorsFilterEmbeddable> generatorsWithFixedSupply;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @OrderColumn(name = "pos_generators_frequency")
    private List<GeneratorsFrequencyReserveEntity> generatorsFrequencyReserve;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @OrderColumn(name = "pos_generators_ordering")
    private List<GeneratorsOrderingEntity> generatorsOrdering;

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
        generatorsWithoutOutage = toEmbeddableGeneratorsFilters(generationDispatchInfos.getGeneratorsWithoutOutage());
        generatorsWithFixedSupply = toEmbeddableGeneratorsFilters(generationDispatchInfos.getGeneratorsWithFixedSupply());

        List<GeneratorsFrequencyReserveEntity> frequencyReserveEntities = toEmbeddableGeneratorsFrequencyReserve(generationDispatchInfos.getGeneratorsFrequencyReserve());
        if (generatorsFrequencyReserve == null) {
            generatorsFrequencyReserve = frequencyReserveEntities;
        } else {
            generatorsFrequencyReserve.clear();
            if (frequencyReserveEntities != null) {
                generatorsFrequencyReserve.addAll(frequencyReserveEntities);
            }
        }

        List<GeneratorsOrderingEntity> orderingEntities = toSubstationsGeneratorsOrdering(generationDispatchInfos.getSubstationsGeneratorsOrdering());
        if (generatorsOrdering == null) {
            generatorsOrdering = orderingEntities;
        } else {
            generatorsOrdering.clear();
            if (orderingEntities != null) {
                generatorsOrdering.addAll(orderingEntities);
            }
        }
    }

    public static List<GeneratorsFilterEmbeddable> toEmbeddableGeneratorsFilters(List<GeneratorsFilterInfos> generators) {
        return generators == null ? null : generators.stream()
            .map(generator -> new GeneratorsFilterEmbeddable(generator.getId(), generator.getName()))
            .collect(Collectors.toList());
    }

    public static List<GeneratorsFrequencyReserveEntity> toEmbeddableGeneratorsFrequencyReserve(List<GeneratorsFrequencyReserveInfos> generators) {
        List<GeneratorsFrequencyReserveEntity> generatorsFrequencyReserveEntities = null;
        if (generators != null) {
            generatorsFrequencyReserveEntities = generators.stream().map(generator -> {
                List<GeneratorsFilterEmbeddable> generatorsFilterEmbeddables = generator.getGeneratorsFilters().stream().map(filter ->
                    new GeneratorsFilterEmbeddable(filter.getId(), filter.getName())).collect(Collectors.toList());
                return new GeneratorsFrequencyReserveEntity(generatorsFilterEmbeddables, generator.getFrequencyReserve());
            }).collect(Collectors.toList());
        }
        return generatorsFrequencyReserveEntities;
    }

    private List<GeneratorsFilterInfos> toGeneratorsFilters(List<GeneratorsFilterEmbeddable> generatorsFilters) {
        return generatorsFilters != null ? generatorsFilters
                .stream()
                .map(generator -> new GeneratorsFilterInfos(generator.getId(), generator.getName()))
                .collect(Collectors.toList()) : null;
    }

    private List<GeneratorsFrequencyReserveInfos> toGeneratorsFrequencyReserve(List<GeneratorsFrequencyReserveEntity> generatorsFrequencyReserve) {
        List<GeneratorsFrequencyReserveInfos> generatorsFrequencyReserveInfos = null;
        if (generatorsFrequencyReserve != null) {
            generatorsFrequencyReserveInfos = generatorsFrequencyReserve.stream()
                .map(generator -> {
                    List<GeneratorsFilterInfos> generatorsFilterInfos = generator.getGeneratorsFilters().stream().map(filter ->
                        new GeneratorsFilterInfos(filter.getId(), filter.getName())).collect(Collectors.toList());
                    return new GeneratorsFrequencyReserveInfos(generatorsFilterInfos, generator.getFrequencyReserve());
                }).collect(Collectors.toList());
        }
        return generatorsFrequencyReserveInfos;
    }

    public static List<GeneratorsOrderingEntity> toSubstationsGeneratorsOrdering(List<SubstationsGeneratorsOrderingInfos> substations) {
        List<GeneratorsOrderingEntity> substationsGeneratorsOrderingEntities = null;
        if (substations != null) {
            substationsGeneratorsOrderingEntities = substations.stream().map(substation ->
                new GeneratorsOrderingEntity(substation.getSubstationIds())
            ).collect(Collectors.toList());
        }
        return substationsGeneratorsOrderingEntities;
    }

    private List<SubstationsGeneratorsOrderingInfos> toSubstationsGeneratorsOrderingInfos(List<GeneratorsOrderingEntity> generatorsOrdering) {
        List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrderingInfos = null;
        if (!CollectionUtils.isEmpty(generatorsOrdering)) {
            substationsGeneratorsOrderingInfos = generatorsOrdering.stream()
                .map(generator -> new SubstationsGeneratorsOrderingInfos(generator.getSubstationIds())).collect(Collectors.toList());
        }
        return substationsGeneratorsOrderingInfos;
    }

    @Override
    public GenerationDispatchInfos toModificationInfos() {
        return GenerationDispatchInfos.builder()
                .date(getDate())
                .uuid(getId())
                .lossCoefficient(getLossCoefficient())
                .defaultOutageRate(getDefaultOutageRate())
                .generatorsWithoutOutage(toGeneratorsFilters(generatorsWithoutOutage))
                .generatorsWithFixedSupply(toGeneratorsFilters(generatorsWithFixedSupply))
                .generatorsFrequencyReserve(toGeneratorsFrequencyReserve(generatorsFrequencyReserve))
                .substationsGeneratorsOrdering(toSubstationsGeneratorsOrderingInfos(generatorsOrdering))
                .build();
    }
}
