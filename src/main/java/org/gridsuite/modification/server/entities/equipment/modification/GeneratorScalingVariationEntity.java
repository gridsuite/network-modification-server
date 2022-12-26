package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfo;
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

    @ElementCollection
    @CollectionTable(name = "ScalingFilterIds")
    private List<String> filterIds;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "VariationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;

    public GeneratorScalingVariation toGeneratorScalingVariation() {
        return GeneratorScalingVariation.builder()
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .filters(getFilterIds().stream()
                        .map(FilterInfo::new)
                        .collect(Collectors.toList()))
                .build();
    }
}
