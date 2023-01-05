package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;

import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.stream.Collectors;

@NoArgsConstructor
@Entity
@Table(name = "GeneratorScalingVariation")
public class GeneratorScalingVariationEntity extends BasicScalingVariationEntity {

    @Override
    public GeneratorScalingVariation toScalingVariation() {
        return GeneratorScalingVariation.builder()
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId().toString(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
