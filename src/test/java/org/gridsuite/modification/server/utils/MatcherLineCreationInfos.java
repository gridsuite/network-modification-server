/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.hamcrest.Description;

import java.util.Objects;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
public class MatcherLineCreationInfos extends MatcherModificationInfos<LineCreationInfos> {

    public static MatcherLineCreationInfos createMatcherLineCreationInfos(LineCreationInfos lineCreationInfos) {
        return new MatcherLineCreationInfos(lineCreationInfos);
    }

    protected MatcherLineCreationInfos(LineCreationInfos ref) {
        super(ref);
    }

    public boolean matchesCurrentLimitsInfos(CurrentLimitsInfos cl1, CurrentLimitsInfos cl2) {
        return (cl1 == null && cl2 == null)
            || (cl1 != null && cl2 != null && cl1.getPermanentLimit() != null && cl2.getPermanentLimit() != null && cl1.getPermanentLimit().equals(cl2.getPermanentLimit()))
            || (cl1 != null && cl2 != null && cl1.getPermanentLimit() == null && cl2.getPermanentLimit() == null);
    }

    @Override
    public boolean matchesSafely(LineCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(reference.getEquipmentId())
            && Objects.equals(m.getEquipmentName(), reference.getEquipmentName())
            && m.getSeriesResistance() == reference.getSeriesResistance()
            && m.getSeriesReactance() == reference.getSeriesReactance()
            && m.getShuntConductance1() != null && m.getShuntConductance1().equals(reference.getShuntConductance1())
            || m.getShuntConductance1() == null && reference.getShuntConductance1() == null
            && m.getShuntSusceptance1() != null && m.getShuntSusceptance1().equals(reference.getShuntSusceptance1())
            || m.getShuntSusceptance1() == null && reference.getShuntSusceptance1() == null
            && m.getShuntConductance2() != null && m.getShuntConductance2().equals(reference.getShuntConductance2())
            || m.getShuntConductance2() == null && reference.getShuntConductance2() == null
            && m.getShuntSusceptance2() != null && m.getShuntSusceptance2().equals(reference.getShuntSusceptance2())
            || m.getShuntSusceptance2() == null && reference.getShuntSusceptance2() == null
            && Objects.equals(m.getVoltageLevelId1(), reference.getVoltageLevelId1())
            && Objects.equals(m.getBusOrBusbarSectionId1(), reference.getBusOrBusbarSectionId1())
            && Objects.equals(m.getVoltageLevelId2(), reference.getVoltageLevelId2())
            && Objects.equals(m.getBusOrBusbarSectionId2(), reference.getBusOrBusbarSectionId2())
            && matchesCurrentLimitsInfos(m.getCurrentLimits1(), reference.getCurrentLimits1())
            && matchesCurrentLimitsInfos(m.getCurrentLimits2(), reference.getCurrentLimits2());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
