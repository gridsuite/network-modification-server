/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LineModificationInfos;
import org.hamcrest.Description;

import java.util.Objects;

import static org.gridsuite.modification.server.utils.MatcherUtils.matchesCurrentLimits;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class MatcherLineModificationInfos extends MatcherModificationInfos<LineModificationInfos> {

    protected MatcherLineModificationInfos(LineModificationInfos ref) {
        super(ref);
    }

    public static MatcherLineModificationInfos createMatcherLineModificationInfos(LineModificationInfos lineModificationInfos) {
        return new MatcherLineModificationInfos(lineModificationInfos);
    }

    @Override
    public boolean matchesSafely(LineModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && Objects.equals(m.getEquipmentName(), reference.getEquipmentName())
                && Objects.equals(m.getSeriesReactance(), reference.getSeriesReactance())
                && Objects.equals(m.getSeriesResistance(), reference.getSeriesResistance())
                && Objects.equals(m.getShuntConductance1(), reference.getShuntConductance1())
                && Objects.equals(m.getShuntSusceptance1(), reference.getShuntSusceptance1())
                && Objects.equals(m.getShuntConductance2(), reference.getShuntConductance2())
                && Objects.equals(m.getShuntSusceptance2(), reference.getShuntSusceptance2())
                && matchesCurrentLimits(m.getCurrentLimits1(), reference.getCurrentLimits1())
                && matchesCurrentLimits(m.getCurrentLimits2(), reference.getCurrentLimits2());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
