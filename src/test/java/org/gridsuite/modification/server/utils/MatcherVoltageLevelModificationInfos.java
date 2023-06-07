/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.VoltageLevelModificationInfos;

import java.util.Objects;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class MatcherVoltageLevelModificationInfos extends MatcherModificationInfos<VoltageLevelModificationInfos> {
    public MatcherVoltageLevelModificationInfos(VoltageLevelModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(VoltageLevelModificationInfos m) {
        return Objects.equals(m.getEquipmentId(), getReference().getEquipmentId()) &&
                Objects.equals(m.getEquipmentName(), getReference().getEquipmentName()) &&
                Objects.equals(m.getNominalVoltage(), getReference().getNominalVoltage()) &&
                Objects.equals(m.getLowVoltageLimit(), getReference().getLowVoltageLimit()) &&
                Objects.equals(m.getHighVoltageLimit(), getReference().getHighVoltageLimit()) &&
                Objects.equals(m.getIpMin(), getReference().getIpMin()) &&
                Objects.equals(m.getIpMax(), getReference().getIpMax());
    }
}
