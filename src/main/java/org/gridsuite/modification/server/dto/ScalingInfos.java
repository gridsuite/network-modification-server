package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.VariationType;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
public class ScalingInfos extends ModificationInfos {
    @Schema(description = "scaling variations")
    private List<AbstractScalingVariationInfos> variations;

    @Schema(description = "variation type")
    private VariationType variationType;
}
