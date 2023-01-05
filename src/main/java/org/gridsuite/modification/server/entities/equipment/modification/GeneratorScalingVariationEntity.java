package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Builder;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariationInfos;

import javax.persistence.Entity;
import java.util.stream.Collectors;

@NoArgsConstructor
@Entity
public class GeneratorScalingVariationEntity extends BasicScalingVariationEntity {

    @Builder
    public GeneratorScalingVariationEntity(GeneratorScalingVariationInfos variationInfos) {
        super(variationInfos.getId(),
              variationInfos.getFilters().stream().map(VariationFilterEntity::new).collect(Collectors.toList()),
              variationInfos.getVariationValue(),
              variationInfos.getVariationMode());
    }

    @Override
    public GeneratorScalingVariationInfos toScalingVariation() {
        return GeneratorScalingVariationInfos.builder()
                .id(getId())
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId().toString(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
