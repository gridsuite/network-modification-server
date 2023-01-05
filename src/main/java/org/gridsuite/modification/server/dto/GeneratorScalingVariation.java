package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingVariationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;

import java.util.UUID;
import java.util.stream.Collectors;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Generator scaling variation")
public class GeneratorScalingVariation extends AbstractScalingVariationInfos {

    @Override
    public GeneratorScalingVariationEntity toEntity() {
        return (GeneratorScalingVariationEntity) GeneratorScalingVariationEntity.builder()
                .filters(getFilters()
                        .stream()
                        .map(info -> VariationFilterEntity.builder()
                            .filterId(UUID.fromString(info.getId()))
                            .name(info.getName())
                        .build())
                        .collect(Collectors.toList()))
                .variationValue(getVariationValue())
                .variationMode(getVariationMode())
                .build();
    }
}
