/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.HvdcLine;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.VscCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.VscCreation;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "VSC creation")
@JsonTypeName("VSC_CREATION")
@ModificationErrorTypeName("CREATE_VSC_ERROR")
public class VscCreationInfos extends EquipmentCreationInfos {
    @Schema(description = "DC nominal voltage")
    private Double nominalV;

    @Schema(description = "DC resistance")
    private Double r;

    @Schema(description = "Maximum active power ")
    private Double maxP;

    @Schema(description = "Operator active power limit (Side1->Side2)")
    private Float operatorActivePowerLimitFromSide1ToSide2;

    @Schema(description = "Operator active power limit (Side2->Side1)")
    private Float operatorActivePowerLimitFromSide2ToSide1;

    @Schema(description = "Converters mode")
    private HvdcLine.ConvertersMode convertersMode;

    @Schema(description = "Active power setpoint")
    private Double activePowerSetpoint;

    @Schema(description = "Angle droop active power control ")
    private Boolean angleDroopActivePowerControl;

    @Schema(description = "p0")
    private Float p0;

    @Schema(description = "droop")
    private Float droop;

    @Schema(description = "Converter station 1")
    private ConverterStationCreationInfos converterStation1;

    @Schema(description = "Converter station 2")
    private ConverterStationCreationInfos converterStation2;

    @Override
    public VscCreationEntity toEntity() {
        return new VscCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new VscCreation(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Vsc creation ${vscId}", "vscId", this.getEquipmentId());
    }
}
