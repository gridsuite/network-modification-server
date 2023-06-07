/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LineModificationInfos;

import java.util.Objects;

import static org.gridsuite.modification.server.utils.MatcherUtils.matchesCurrentLimits;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class MatcherLineModificationInfos extends MatcherModificationInfos<LineModificationInfos> {
    public MatcherLineModificationInfos(LineModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LineModificationInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(m.getEquipmentId(), getReference().getEquipmentId())
                && Objects.equals(m.getEquipmentName(), getReference().getEquipmentName())
                && Objects.equals(m.getSeriesReactance(), getReference().getSeriesReactance())
                && Objects.equals(m.getSeriesResistance(), getReference().getSeriesResistance())
                && Objects.equals(m.getShuntConductance1(), getReference().getShuntConductance1())
                && Objects.equals(m.getShuntSusceptance1(), getReference().getShuntSusceptance1())
                && Objects.equals(m.getShuntConductance2(), getReference().getShuntConductance2())
                && Objects.equals(m.getShuntSusceptance2(), getReference().getShuntSusceptance2())
                && matchesCurrentLimits(m.getCurrentLimits1(), getReference().getCurrentLimits1())
                && matchesCurrentLimits(m.getCurrentLimits2(), getReference().getCurrentLimits2());
    }
}
