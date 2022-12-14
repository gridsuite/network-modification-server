/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;

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

    public enum ActionType {
        LOCKOUT,
        TRIP,
        SWITCH_ON,
        ENERGISE_END_ONE,
        ENERGISE_END_TWO
    }

    @Override
    public void check() {
        super.check();
        if (action == null) {
            throw new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY);
        }
    }
}
