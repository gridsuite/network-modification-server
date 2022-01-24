/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import java.util.Objects;

import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

public class MatcherVoltageLevelCreationInfos extends MatcherEquipmentModificationInfos<VoltageLevelCreationInfos> {
    protected MatcherVoltageLevelCreationInfos(VoltageLevelCreationInfos ref) {
        super(ref);
    }

    public static MatcherVoltageLevelCreationInfos createMatcherVoltageLevelCreationInfos(VoltageLevelCreationInfos ref) {
        return new MatcherVoltageLevelCreationInfos(ref);
    }

    public boolean matchesSafely(VoltageLevelCreationInfos m) {
        return super.matchesSafely(m)
            && Math.abs(reference.getNominalVoltage() - m.getNominalVoltage()) < 0.2
            && Objects.equals(reference.getSubstationId(), m.getSubstationId());
    }
}
