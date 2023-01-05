package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.entities.equipment.modification.ScalingVariationEntity;

import java.util.List;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Scaling creation")
public class ScalingVariationInfos {
    @Schema(description = "id")
    UUID id;

    @Schema(description = "filters")
    List<FilterInfos> filters;

    @Schema(description = "variation mode")
    VariationMode variationMode;

    @Schema(description = "variation value")
    Double variationValue;

    @Schema(description = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

    public ScalingVariationEntity toEntity() {
        return new ScalingVariationEntity(this);
    }
}
