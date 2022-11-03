/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.PhaseTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.RatioTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;
import org.hamcrest.Description;

import java.util.Objects;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */
public class MatcherTwoWindingsTransformerCreationInfos extends MatcherModificationInfos<TwoWindingsTransformerCreationInfos> {

    public static MatcherTwoWindingsTransformerCreationInfos createMatcherTwoWindingsTransformerCreationInfos(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        return new MatcherTwoWindingsTransformerCreationInfos(twoWindingsTransformerCreationInfos);
    }

    protected MatcherTwoWindingsTransformerCreationInfos(TwoWindingsTransformerCreationInfos ref) {
        super(ref);
    }

    private boolean matchesCurrentLimitsInfos(CurrentLimitsInfos cl1, CurrentLimitsInfos cl2) {
        return (cl1 == null && cl2 == null)
                || (cl1 != null && cl2 != null && cl1.getPermanentLimit() != null && cl2.getPermanentLimit() != null && cl1.getPermanentLimit().equals(cl2.getPermanentLimit()))
                || (cl1 != null && cl2 != null && cl1.getPermanentLimit() == null && cl2.getPermanentLimit() == null);
    }

    private boolean matchesRatioTapChangerInfos(RatioTapChangerCreationInfos rt1, RatioTapChangerCreationInfos rt2) {
        return (rt1 == null && rt2 == null)
                || (rt1 != null && rt2 != null && rt1.isLoadTapChangingCapabilities() == rt2.isLoadTapChangingCapabilities() && rt1.isRegulating() == rt2.isRegulating() && areDoublesEqual(rt1.getTargetV(), rt2.getTargetV()) && areDoublesEqual(rt1.getTargetDeadband(), rt2.getTargetDeadband()) && Objects.equals(rt1.getRegulatingTerminalVlId(), rt2.getRegulatingTerminalVlId()) && Objects.equals(rt1.getRegulatingTerminalType(), rt2.getRegulatingTerminalType()) && Objects.equals(rt1.getRegulatingTerminalId(), rt2.getRegulatingTerminalId()) && rt1.getLowTapPosition() == rt2.getLowTapPosition() && rt1.getTapPosition() == rt2.getTapPosition());
    }

    private boolean matchesPhaseTapChangerInfos(PhaseTapChangerCreationInfos pt1, PhaseTapChangerCreationInfos pt2) {
        return (pt1 == null && pt2 == null)
                || (pt1 != null && pt2 != null && pt1.getRegulationMode().equals(pt2.getRegulationMode()) && pt1.isRegulating() == pt2.isRegulating() && pt1.getRegulationMode() == pt2.getRegulationMode() && areDoublesEqual(pt1.getRegulationValue(), pt2.getRegulationValue()) && areDoublesEqual(pt1.getTargetDeadband(), pt2.getTargetDeadband()) && Objects.equals(pt1.getRegulatingTerminalVlId(), pt2.getRegulatingTerminalVlId()) && Objects.equals(pt1.getRegulatingTerminalType(), pt2.getRegulatingTerminalType()) && Objects.equals(pt1.getRegulatingTerminalId(), pt2.getRegulatingTerminalId()) && pt1.getLowTapPosition() == pt2.getLowTapPosition() && pt1.getTapPosition() == pt2.getTapPosition());
    }

    @Override
    public boolean matchesSafely(TwoWindingsTransformerCreationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && m.getSubstationIds().equals(reference.getSubstationIds())
                && m.getEquipmentName().equals(reference.getEquipmentName())
                && m.getVoltageLevelId1().equals(reference.getVoltageLevelId1())
                && m.getVoltageLevelId2().equals(reference.getVoltageLevelId2())
                && m.getBusOrBusbarSectionId1().equals(reference.getBusOrBusbarSectionId1())
                && m.getBusOrBusbarSectionId2().equals(reference.getBusOrBusbarSectionId2())
                && m.getRatedVoltage1() == reference.getRatedVoltage1()
                && m.getRatedVoltage2() == reference.getRatedVoltage2()
                && m.getMagnetizingSusceptance() == reference.getMagnetizingSusceptance()
                && m.getMagnetizingConductance() == reference.getMagnetizingConductance()
                && m.getSeriesReactance() == reference.getSeriesReactance()
                && m.getSeriesResistance() == reference.getSeriesResistance()
                && areDoublesEqual(m.getRatedS(), reference.getRatedS())
                && matchesCurrentLimitsInfos(m.getCurrentLimits1(), reference.getCurrentLimits1())
                && matchesCurrentLimitsInfos(m.getCurrentLimits2(), reference.getCurrentLimits2())
                && matchesRatioTapChangerInfos(m.getRatioTapChanger(), reference.getRatioTapChanger())
                && matchesPhaseTapChangerInfos(m.getPhaseTapChanger(), reference.getPhaseTapChanger());
    }

    private boolean areDoublesEqual(Double a, Double b) {
        return (a == null && b == null) || (a != null && b != null && (Math.abs(a - b) < EPSILON));
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
