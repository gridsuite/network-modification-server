package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Entity
@Builder
@Table(name = "GeneratorScalingVariation")
public class GeneratorScalingVariationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @ManyToMany(cascade = CascadeType.ALL)
    private List<VariationFilterEntity> filters;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "VariationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;

    public GeneratorScalingVariation toGeneratorScalingVariation() {
        return GeneratorScalingVariation.builder()
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId().toString(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
