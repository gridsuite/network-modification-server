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
import org.gridsuite.modification.server.entities.equipment.modification.OperationalStatusModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.OperationalStatusModification;
import java.util.HashMap;
import java.util.Map;

import static org.gridsuite.modification.server.NetworkModificationException.Type.OPERATIONAL_STATUS_TYPE_EMPTY;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Operational status modification")
@JsonTypeName("OPERATIONAL_STATUS_MODIFICATION")
@ModificationErrorTypeName("OPERATIONAL_STATUS_ERROR")
public class OperationalStatusModificationInfos extends EquipmentModificationInfos {
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
    public OperationalStatusModificationEntity toEntity() {
        return new OperationalStatusModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new OperationalStatusModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        String defaultName;
        switch (action) {
            case LOCKOUT:
                defaultName = "Lockout ${equipmentId}";
                break;
            case TRIP:
                defaultName = "Trip ${equipmentId}";
                break;
            case ENERGISE_END_ONE:
            case ENERGISE_END_TWO:
                defaultName = "Energise ${equipmentId}";
                break;
            case SWITCH_ON:
                defaultName = "Switch on ${equipmentId}";
                break;
            default:
                defaultName = "";
        }
        return reporter.createSubReporter(getType().name() + "_" + action, defaultName, "equipmentId", this.getEquipmentId());
    }

    @Override
    public void check() {
        super.check();
        if (action == null) {
            throw new NetworkModificationException(OPERATIONAL_STATUS_TYPE_EMPTY);
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
