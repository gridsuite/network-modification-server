/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.ConverterStationModificationEntity;

import java.util.List;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Converter station modification")
@JsonTypeName("CONVERTER_STATION_MODIFICATION")
@ModificationErrorTypeName("MODIFY_CONVERTER_STATION_ERROR")
public class ConverterStationModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Loss Factor")
    private AttributeModification<Float> lossFactor;

    @Schema(description = "Reactive power")
    private AttributeModification<Double> reactivePower;

    @Schema(description = "Voltage regulation")
    private AttributeModification<Boolean> voltageRegulationOn;

    @Schema(description = "Voltage")
    private AttributeModification<Double> voltage;

    @Schema(description = "Reactive capability curve")
    private AttributeModification<Boolean> reactiveCapabilityCurve;

    @Schema(description = "Minimum reactive power")
    private AttributeModification<Double> minimumReactivePower;

    @Schema(description = "Maximum reactive power")
    private AttributeModification<Double> maximumReactivePower;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveModificationInfos> reactiveCapabilityCurvePoints;

    @Override
    public ConverterStationModificationEntity toEntity() {
        return new ConverterStationModificationEntity(this);
    }
}
