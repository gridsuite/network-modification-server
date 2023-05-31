/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.TwoWindingsTransformerModificationInfos;
import org.hamcrest.Description;

import java.util.Objects;

import static org.gridsuite.modification.server.utils.MatcherUtils.matchesCurrentLimits;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public class MatcherTwoWindingsTransformerModificationInfos extends MatcherModificationInfos<TwoWindingsTransformerModificationInfos> {

    protected MatcherTwoWindingsTransformerModificationInfos(TwoWindingsTransformerModificationInfos ref) {
        super(ref);
    }

    public static MatcherTwoWindingsTransformerModificationInfos createMatcherTwoWindingsTransformerModificationInfos(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos) {
        return new MatcherTwoWindingsTransformerModificationInfos(twoWindingsTransformerModificationInfos);
    }

    @Override
    public boolean matchesSafely(TwoWindingsTransformerModificationInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(m.getEquipmentId(), reference.getEquipmentId())
                && Objects.equals(m.getEquipmentName(), reference.getEquipmentName())
                && Objects.equals(m.getRatedVoltage1(), reference.getRatedVoltage1())
                && Objects.equals(m.getRatedVoltage2(), reference.getRatedVoltage2())
                && Objects.equals(m.getMagnetizingSusceptance(), reference.getMagnetizingSusceptance())
                && Objects.equals(m.getMagnetizingConductance(), reference.getMagnetizingConductance())
                && Objects.equals(m.getSeriesReactance(), reference.getSeriesReactance())
                && Objects.equals(m.getSeriesResistance(), reference.getSeriesResistance())
                && Objects.equals(m.getRatedS(), reference.getRatedS())
                && matchesCurrentLimits(m.getCurrentLimits1(), reference.getCurrentLimits1())
                && matchesCurrentLimits(m.getCurrentLimits2(), reference.getCurrentLimits2());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
