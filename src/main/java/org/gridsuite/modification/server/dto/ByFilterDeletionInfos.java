/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
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
import org.gridsuite.modification.server.entities.equipment.deletion.ByFilterDeletionEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.ByFilterDeletion;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "By filter deletion")
@JsonTypeName("BY_FILTER_DELETION")
@ModificationErrorTypeName("BY_FILTER_DELETION_ERROR")
public class ByFilterDeletionInfos extends ModificationInfos {
    @Schema(description = "Equipment type")
    private String equipmentType;

    @Schema(description = "Equipment filters")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<FilterInfos> equipmentFilters;

    @Override
    public ByFilterDeletionEntity toEntity() {
        return new ByFilterDeletionEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new ByFilterDeletion(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "By filter deletion");
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("equipmentType", getEquipmentType());
        return mapMessageValues;
    }
}
