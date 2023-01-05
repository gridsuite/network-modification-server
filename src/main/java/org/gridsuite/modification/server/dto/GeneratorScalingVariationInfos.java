package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingVariationEntity;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Generator scaling variation")
public class GeneratorScalingVariationInfos extends ScalingVariationInfos {

    @Override
    public GeneratorScalingVariationEntity toEntity() {
        return new GeneratorScalingVariationEntity(this);
    }
}
