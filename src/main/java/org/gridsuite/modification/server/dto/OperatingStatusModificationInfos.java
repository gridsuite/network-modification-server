/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
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
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.OperatingStatusModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.OperatingStatusModification;
import java.util.HashMap;
import java.util.Map;

import static org.gridsuite.modification.server.NetworkModificationException.Type.OPERATING_ACTION_TYPE_EMPTY;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Operating status modification")
@JsonTypeName("OPERATING_STATUS_MODIFICATION")
@ModificationErrorTypeName("OPERATING_STATUS_MODIFICATION_ERROR")
public class OperatingStatusModificationInfos extends EquipmentModificationInfos {
    @Schema(description = "Action type")
    private ActionType action;

    @Schema(description = "Energized end one or two voltage level ID")
    private String energizedVoltageLevelId;

    public enum ActionType {
        LOCKOUT,
        TRIP,
        SWITCH_ON,
        ENERGISE_END_ONE,
        ENERGISE_END_TWO
    }

    @Override
    public OperatingStatusModificationEntity toEntity() {
        return new OperatingStatusModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new OperatingStatusModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        String defaultName = switch (action) {
            case LOCKOUT -> "Lockout ${equipmentId}";
            case TRIP -> "Trip ${equipmentId}";
            case ENERGISE_END_ONE, ENERGISE_END_TWO -> "Energise ${equipmentId}";
            case SWITCH_ON -> "Switch on ${equipmentId}";
        };
        return reporter.createSubReporter(getType().name() + "_" + action, defaultName, "equipmentId", this.getEquipmentId());
    }

    @Override
    public void check() {
        super.check();
        if (action == null) {
            throw new NetworkModificationException(OPERATING_ACTION_TYPE_EMPTY);
        }
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("action", getAction().name());
        mapMessageValues.put("equipmentId", getEquipmentId());
        if (getEnergizedVoltageLevelId() != null) {
            mapMessageValues.put("energizedVoltageLevelId", getEnergizedVoltageLevelId());
        }
        return mapMessageValues;

    }
}
