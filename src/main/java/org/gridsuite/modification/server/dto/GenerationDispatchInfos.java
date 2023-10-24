/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.GenerationDispatchEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.GenerationDispatch;

import java.util.List;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Generation dispatch creation")
@JsonTypeName("GENERATION_DISPATCH")
@ModificationErrorTypeName("GENERATION_DISPATCH_ERROR")
public class GenerationDispatchInfos extends ModificationInfos {
    @Schema(description = "loss coefficient")
    private Double lossCoefficient;

    @Schema(description = "default outage rate")
    private Double defaultOutageRate;

    @Schema(description = "generators without outage")
    private List<GeneratorsFilterInfos> generatorsWithoutOutage;

    @Schema(description = "generators with fixed supply")
    private List<GeneratorsFilterInfos> generatorsWithFixedSupply;

    @Schema(description = "generators frequency reserve")
    private List<GeneratorsFrequencyReserveInfos> generatorsFrequencyReserve;

    @Schema(description = "substations hierarchy for ordering generators with marginal cost")
    private List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrdering;

    @Override
    public GenerationDispatchEntity toEntity() {
        return new GenerationDispatchEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new GenerationDispatch(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Generation dispatch");
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        return Map.of();
    }
}
