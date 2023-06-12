/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

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
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.BranchStatusModification;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_TYPE_EMPTY;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Branch status modification")
public class BranchStatusModificationInfos extends EquipmentModificationInfos {

    @Schema(description = "Action type")
    ActionType action;

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
    public BranchStatusModificationEntity toEntity() {
        return new BranchStatusModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new BranchStatusModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.BRANCH_ACTION_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        String defaultName;
        switch (action) {
            case LOCKOUT:
                defaultName = "Lockout ${branchId}";
                break;
            case TRIP:
                defaultName = "Trip ${branchId}";
                break;
            case ENERGISE_END_ONE:
            case ENERGISE_END_TWO:
                defaultName = "Energise ${branchId}";
                break;
            case SWITCH_ON:
                defaultName = "Switch on ${branchId}";
                break;
            default:
                defaultName = "";
        }
        return reporter.createSubReporter(ModificationType.BRANCH_STATUS_MODIFICATION.name() + "_" + action, defaultName, "branchId", this.getEquipmentId());
    }

    @Override
    public void check() {
        super.check();
        if (action == null) {
            throw new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY);
        }
    }
}
