package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingVariationEntity;

import java.util.List;
import java.util.stream.Collectors;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Generator scaling variation")
public class GeneratorScalingVariation {
    List<FilterInfo> filters;
    VariationMode variationMode;
    Double variationValue;

    public GeneratorScalingVariationEntity toEntity() {
        return GeneratorScalingVariationEntity.builder()
                .filterIds(getFilters().stream().map(FilterInfo::getId).collect(Collectors.toList()))
                .variationValue(getVariationValue())
                .variationMode(getVariationMode())
                .build();
    }
}
