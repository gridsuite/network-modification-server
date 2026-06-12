/*
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.CascadeType;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OrderColumn;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.GenerationDispatchInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.model.GeneratorsFilterModel;
import org.gridsuite.modification.model.GeneratorsFrequencyReserveModel;
import org.gridsuite.modification.model.SubstationsGeneratorsOrderingModel;
import org.gridsuite.modification.server.entities.ModificationEntity;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
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
    @OrderColumn(name = "pos_generators_frequency", nullable = false, columnDefinition = "integer default 0")
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

    public static List<GeneratorsFilterEmbeddable> toEmbeddableGeneratorsFilters(List<GeneratorsFilterModel> generators) {
        return generators == null ? null : generators.stream()
            .map(generator -> new GeneratorsFilterEmbeddable(generator.getId(), generator.getName()))
            .collect(Collectors.toList());
    }

    public static List<GeneratorsFrequencyReserveEntity> toEmbeddableGeneratorsFrequencyReserve(List<GeneratorsFrequencyReserveModel> generators) {
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

    private List<GeneratorsFilterModel> toGeneratorsFilters(List<GeneratorsFilterEmbeddable> generatorsFilters) {
        return generatorsFilters != null ? generatorsFilters
                .stream()
                .map(generator -> new GeneratorsFilterModel(generator.getId(), generator.getName()))
                .collect(Collectors.toList()) : null;
    }

    private List<GeneratorsFrequencyReserveModel> toGeneratorsFrequencyReserve(List<GeneratorsFrequencyReserveEntity> generatorsFrequencyReserve) {
        List<GeneratorsFrequencyReserveModel> generatorsFrequencyReserveInfos = null;
        if (generatorsFrequencyReserve != null) {
            generatorsFrequencyReserveInfos = generatorsFrequencyReserve.stream()
                .filter(Objects::nonNull)
                .map(generator -> {
                    List<GeneratorsFilterModel> generatorsFilterInfos = generator.getGeneratorsFilters().stream().map(filter ->
                        new GeneratorsFilterModel(filter.getId(), filter.getName())).collect(Collectors.toList());
                    return new GeneratorsFrequencyReserveModel(generatorsFilterInfos, generator.getFrequencyReserve());
                }).collect(Collectors.toList());
        }
        return generatorsFrequencyReserveInfos;
    }

    public static List<GeneratorsOrderingEntity> toSubstationsGeneratorsOrdering(List<SubstationsGeneratorsOrderingModel> substations) {
        List<GeneratorsOrderingEntity> substationsGeneratorsOrderingEntities = null;
        if (substations != null) {
            substationsGeneratorsOrderingEntities = substations.stream().map(substation ->
                new GeneratorsOrderingEntity(new ArrayList<>(substation.getSubstationIds()))
            ).collect(Collectors.toList());
        }
        return substationsGeneratorsOrderingEntities;
    }

    private List<SubstationsGeneratorsOrderingModel> toSubstationsGeneratorsOrderingInfos(List<GeneratorsOrderingEntity> generatorsOrdering) {
        List<SubstationsGeneratorsOrderingModel> substationsGeneratorsOrderingInfos = null;
        if (generatorsOrdering != null) {
            substationsGeneratorsOrderingInfos = generatorsOrdering.stream()
                .map(generator -> new SubstationsGeneratorsOrderingModel(generator.getSubstationIds())).collect(Collectors.toList());
        }
        return substationsGeneratorsOrderingInfos;
    }

    @Override
    public GenerationDispatchInfos toModificationInfos() {
        return GenerationDispatchInfos.builder()
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .activated(getActivated())
                .description(getDescription())
                .lossCoefficient(getLossCoefficient())
                .defaultOutageRate(getDefaultOutageRate())
                .generatorsWithoutOutage(toGeneratorsFilters(generatorsWithoutOutage))
                .generatorsWithFixedSupply(toGeneratorsFilters(generatorsWithFixedSupply))
                .generatorsFrequencyReserve(toGeneratorsFrequencyReserve(generatorsFrequencyReserve))
                .substationsGeneratorsOrdering(toSubstationsGeneratorsOrderingInfos(generatorsOrdering))
                .build();
    }
}
