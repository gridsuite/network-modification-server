/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.creation.ConverterStationCreationEntity;

import java.util.List;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Converter station creation")
public class ConverterStationCreationInfos implements ReactiveLimitsHolderInfos {
    @Schema(description = "Converter station id")
    private String converterStationId;

    @Schema(description = "Converter name")
    private String converterStationName;

    @Schema(description = "Loss Factor")
    private Float lossFactor;

    @Schema(description = "Reactive power")
    private Double reactivePower;

    @Schema(description = "Voltage regulation")
    private Boolean voltageRegulationOn;

    @Schema(description = "Voltage")
    private Double voltage;

    @Schema(description = "Reactive capability curve")
    private Boolean reactiveCapabilityCurve;

    @Schema(description = "Minimum reactive power")
    private Double minimumReactivePower;

    @Schema(description = "Maximum reactive power")
    private Double maximumReactivePower;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveCreationInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Voltage level id")
    private String voltageLevelId;

    @Schema(description = "Bus id")
    private String busOrBusbarSectionId;

    @Schema(description = "Connection Name")
    private String connectionName;

    @Schema(description = "Connection Direction")
    private ConnectablePosition.Direction connectionDirection;

    @Schema(description = "Connection Position")
    private Integer connectionPosition;

    public ConverterStationCreationEntity toEntity() {
        return new ConverterStationCreationEntity(this);
    }
}
