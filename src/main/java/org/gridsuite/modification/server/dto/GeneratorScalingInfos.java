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
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.GeneratorScaling;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
@JsonTypeName("GENERATOR_SCALING")
@ModificationErrorTypeName("GENERATOR_SCALING_ERROR")
@Schema(description = "Generator scaling creation")
public class GeneratorScalingInfos extends ScalingInfos {
    @Override
    public GeneratorScalingEntity toEntity() {
        return new GeneratorScalingEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new GeneratorScaling(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Generator scaling");
    }
}
