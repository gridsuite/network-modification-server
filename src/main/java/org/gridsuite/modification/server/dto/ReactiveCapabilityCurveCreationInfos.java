package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Schema(description = "Generator reactive capability curve point creation")
public class ReactiveCapabilityCurveCreationInfos {
    @Schema(description = "Minimum reactive power ")
    private Double qminP;

    @Schema(description = "Maximum reactive power")
    private Double qmaxP;

    @Schema(description = "Active Power")
    private Double p;
}
