/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
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
public class ShuntCompensatorModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Maximum number of sections")
    private AttributeModification<Integer> maximumNumberOfSections;

    @Schema(description = "Current number of sections")
    private AttributeModification<Integer> currentNumberOfSections;

    @Schema(description = "Susceptance per section")
    private AttributeModification<Double> susceptancePerSection;

    @JsonProperty("qAtNominalV")
    @Schema(description = "Q at Nominal Voltage")
    private AttributeModification<Double> qAtNominalV;

    @Schema(description = "Shunt Compensator Type")
    private AttributeModification<ShuntCompensatorType> shuntCompensatorType;

    @Schema(description = "Identical sections")
    private AttributeModification<Boolean> isIdenticalSection;

    @Override
    public ShuntCompensatorModificationEntity toEntity() {
        return new ShuntCompensatorModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new ShuntCompensatorModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MODIFY_SHUNT_COMPENSATOR_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.SHUNT_COMPENSATOR_MODIFICATION.name(), "Modification of shunt compensator " + getEquipmentId());
    }
}
