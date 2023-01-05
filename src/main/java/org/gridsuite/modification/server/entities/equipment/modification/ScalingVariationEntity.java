package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "ScalingVariation")
public class ScalingVariationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @ManyToMany(cascade = CascadeType.ALL)
    @JoinTable(
            joinColumns = @JoinColumn(name = "id"),
            inverseJoinColumns = @JoinColumn(name = "filterId"))
    private List<VariationFilterEntity> filters;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "variationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;

    @Column(name = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

    public ScalingVariationEntity(ScalingVariationInfos variationInfos) {
        this.id = null;
        this.filters = getFiltersEntity(variationInfos);
        this.variationMode = variationInfos.getVariationMode();
        this.variationValue = variationInfos.getVariationValue();
        this.reactiveVariationMode = variationInfos.getReactiveVariationMode();
    }

    private List<VariationFilterEntity> getFiltersEntity(ScalingVariationInfos variationInfos) {
        return variationInfos.getFilters().stream().map(filterInfos -> VariationFilterEntity.builder()
                        .filterId(UUID.fromString(filterInfos.getId()))
                        .name(filterInfos.getName())
                        .build())
                .collect(Collectors.toList());
    }

    public ScalingVariationInfos toScalingVariation() {
        return ScalingVariationInfos.builder()
                .id(getId())
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .reactiveVariationMode(getReactiveVariationMode())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId().toString(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
