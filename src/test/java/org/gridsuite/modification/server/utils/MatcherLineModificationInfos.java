/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitCreationInfos;
import org.gridsuite.modification.server.dto.LineModificationInfos;
import org.hamcrest.Description;

import java.util.List;
import java.util.Objects;

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

    private static boolean matchesCurrentLimits(CurrentLimitsInfos limits1, CurrentLimitsInfos limits2) {
        return Objects.equals(limits1.getPermanentLimit(), limits2.getPermanentLimit())
                && matchesTemoraryLimits(limits1.getTemporaryLimits(), limits2.getTemporaryLimits());
    }

    private static boolean matchesTemoraryLimits(List<CurrentTemporaryLimitCreationInfos> tempLimits1, List<CurrentTemporaryLimitCreationInfos> tempLimits2) {
        if (tempLimits1 == null && tempLimits2 == null) {
            return true;
        }
        if (tempLimits1 == null || tempLimits2 == null) {
            if (tempLimits1 == null && tempLimits2 != null && tempLimits2.isEmpty()) {
                return true;
            } else {
                return tempLimits2 == null && tempLimits1 != null && tempLimits1.isEmpty();
            }
        }
        if (tempLimits1.size() != tempLimits2.size()) {
            return false;
        }
        for (int i = 0; i < tempLimits1.size(); i++) {
            if (!Objects.equals(tempLimits1.get(i).getAcceptableDuration(), tempLimits2.get(i).getAcceptableDuration())
                    || !Objects.equals(tempLimits1.get(i).getName(), tempLimits2.get(i).getName())
                    || !Objects.equals(tempLimits1.get(i).getValue(), tempLimits2.get(i).getValue())) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
