package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Converter station creation")
public class ConverterStationCreationInfos extends InjectionWithReactiveLimitsCreationInfos {
    @Schema(description = "Loss Factor")
    private Float lossFactor;

    @Schema(description = "Reactive power")
    private Double reactivePower;

    @Schema(description = "Voltage regulation")
    private Boolean voltageRegulationOn;

    @Schema(description = "Voltage")
    private Double voltage;
}
