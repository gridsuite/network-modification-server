/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;

import java.util.Objects;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
public class MatcherLineCreationInfos extends MatcherModificationInfos<LineCreationInfos> {
    public MatcherLineCreationInfos(LineCreationInfos ref) {
        super(ref);
    }

    public static boolean matchesCurrentLimitsInfos(CurrentLimitsInfos cl1, CurrentLimitsInfos cl2) {
        return (cl1 == null && cl2 == null)
            || (cl1 != null && cl2 != null && cl1.getPermanentLimit() != null && cl2.getPermanentLimit() != null && cl1.getPermanentLimit().equals(cl2.getPermanentLimit()))
            || (cl1 != null && cl2 != null && cl1.getPermanentLimit() == null && cl2.getPermanentLimit() == null);
    }

    @Override
    public boolean matchesSafely(LineCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(getReference().getEquipmentId())
            && Objects.equals(m.getEquipmentName(), getReference().getEquipmentName())
            && m.getSeriesResistance() == getReference().getSeriesResistance()
            && m.getSeriesReactance() == getReference().getSeriesReactance()
            && m.getShuntConductance1() != null && m.getShuntConductance1().equals(getReference().getShuntConductance1())
            || m.getShuntConductance1() == null && getReference().getShuntConductance1() == null
            && m.getShuntSusceptance1() != null && m.getShuntSusceptance1().equals(getReference().getShuntSusceptance1())
            || m.getShuntSusceptance1() == null && getReference().getShuntSusceptance1() == null
            && m.getShuntConductance2() != null && m.getShuntConductance2().equals(getReference().getShuntConductance2())
            || m.getShuntConductance2() == null && getReference().getShuntConductance2() == null
            && m.getShuntSusceptance2() != null && m.getShuntSusceptance2().equals(getReference().getShuntSusceptance2())
            || m.getShuntSusceptance2() == null && getReference().getShuntSusceptance2() == null
            && Objects.equals(m.getVoltageLevelId1(), getReference().getVoltageLevelId1())
            && Objects.equals(m.getBusOrBusbarSectionId1(), getReference().getBusOrBusbarSectionId1())
            && Objects.equals(m.getVoltageLevelId2(), getReference().getVoltageLevelId2())
            && Objects.equals(m.getBusOrBusbarSectionId2(), getReference().getBusOrBusbarSectionId2())
            && matchesCurrentLimitsInfos(m.getCurrentLimits1(), getReference().getCurrentLimits1())
            && matchesCurrentLimitsInfos(m.getCurrentLimits2(), getReference().getCurrentLimits2());
    }
}
