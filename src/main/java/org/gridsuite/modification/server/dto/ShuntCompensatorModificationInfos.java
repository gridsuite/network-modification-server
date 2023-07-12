/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.ShuntCompensatorModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.ShuntCompensatorModification;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Shunt compensator modification")
@JsonTypeName("SHUNT_COMPENSATOR_MODIFICATION")
@ModificationErrorTypeName("MODIFY_SHUNT_COMPENSATOR_ERROR")
public class ShuntCompensatorModificationInfos extends BasicEquipmentModificationInfos {

    @Schema(description = "voltage level id")
    private String voltageLevelId;

    @Schema(description = "Susceptance per section")
    private AttributeModification<Double> susceptancePerSection;

    @JsonProperty("qAtNominalV")
    @Schema(description = "Q at Nominal Voltage")
    private AttributeModification<Double> qAtNominalV;

    @Schema(description = "Shunt Compensator Type")
    private AttributeModification<ShuntCompensatorType> shuntCompensatorType;

    @Override
    public ShuntCompensatorModificationEntity toEntity() {
        return new ShuntCompensatorModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new ShuntCompensatorModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Modification of shunt compensator " + getEquipmentId());
    }
}
