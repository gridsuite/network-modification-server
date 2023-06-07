/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.TwoWindingsTransformerModificationInfos;

import java.util.Objects;

import static org.gridsuite.modification.server.utils.MatcherUtils.matchesCurrentLimits;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public class MatcherTwoWindingsTransformerModificationInfos extends MatcherModificationInfos<TwoWindingsTransformerModificationInfos> {
    public MatcherTwoWindingsTransformerModificationInfos(TwoWindingsTransformerModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(TwoWindingsTransformerModificationInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(m.getEquipmentId(), getReference().getEquipmentId())
                && Objects.equals(m.getEquipmentName(), getReference().getEquipmentName())
                && Objects.equals(m.getRatedVoltage1(), getReference().getRatedVoltage1())
                && Objects.equals(m.getRatedVoltage2(), getReference().getRatedVoltage2())
                && Objects.equals(m.getMagnetizingSusceptance(), getReference().getMagnetizingSusceptance())
                && Objects.equals(m.getMagnetizingConductance(), getReference().getMagnetizingConductance())
                && Objects.equals(m.getSeriesReactance(), getReference().getSeriesReactance())
                && Objects.equals(m.getSeriesResistance(), getReference().getSeriesResistance())
                && Objects.equals(m.getRatedS(), getReference().getRatedS())
                && matchesCurrentLimits(m.getCurrentLimits1(), getReference().getCurrentLimits1())
                && matchesCurrentLimits(m.getCurrentLimits2(), getReference().getCurrentLimits2());
    }
}
