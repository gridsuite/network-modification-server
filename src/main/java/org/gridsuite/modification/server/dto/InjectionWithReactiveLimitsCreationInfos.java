package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
public class InjectionWithReactiveLimitsCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Reactive capability curve")
    private Boolean reactiveCapabilityCurve;

    @Schema(description = "Minimum reactive power")
    private Double minimumReactivePower;

    @Schema(description = "Maximum reactive power")
    private Double maximumReactivePower;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveCreationInfos> reactiveCapabilityCurvePoints;
}
