/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.hamcrest.Description;

import java.util.List;
import java.util.Objects;

public class MatcherBatteryCreationInfos extends MatcherModificationInfos<BatteryCreationInfos> {

    protected MatcherBatteryCreationInfos(BatteryCreationInfos ref) {
        super(ref);
    }

    public static MatcherBatteryCreationInfos createMatcherBatteryCreationInfos(BatteryCreationInfos batteryCreationInfos) {
        return new MatcherBatteryCreationInfos(batteryCreationInfos);
    }

    @Override
    public boolean matchesSafely(BatteryCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(reference.getEquipmentId())
            && StringUtils.equals(m.getEquipmentName(), reference.getEquipmentName())
            && m.getVoltageLevelId().equals(reference.getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId())
            && m.getMinActivePower() == reference.getMinActivePower()
            && m.getMaxActivePower() == reference.getMaxActivePower()
            && m.getActivePowerSetpoint() == reference.getActivePowerSetpoint()
            && Objects.equals(m.getReactivePowerSetpoint(), reference.getReactivePowerSetpoint())
            && Objects.equals(m.getDroop(), reference.getDroop())
            && Objects.equals(m.getParticipate(), reference.getParticipate())
            && Objects.equals(m.getMaximumReactivePower(), reference.getMaximumReactivePower())
            && Objects.equals(m.getMinimumReactivePower(), reference.getMinimumReactivePower())
            && Objects.equals(m.getReactiveCapabilityCurve(), reference.getReactiveCapabilityCurve())
            && matchesReactiveCapabilityCurvePoints(m.getReactiveCapabilityCurvePoints(), reference.getReactiveCapabilityCurvePoints());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }

    private static boolean matchesReactiveCapabilityCurvePoints(List<ReactiveCapabilityCurveCreationInfos> points1, List<ReactiveCapabilityCurveCreationInfos> points2) {
        if (points1 == null && points2 == null) {
            return true;
        }

        if (points1 != null && points2 != null && points1.size() == points2.size()) {
            for (int idx = 0; idx < points1.size(); idx++) {
                if (!matchesReactiveCapabilityCurve(points1.get(idx), points2.get(idx))) {
                    return false;
                }
            }

            return true;
        }

        return false;
    }

    private static boolean matchesReactiveCapabilityCurve(ReactiveCapabilityCurveCreationInfos point1, ReactiveCapabilityCurveCreationInfos point2) {
        return point1 == null && point2 == null ||
               point1 != null && point2 != null &&
                Objects.equals(point1.getP(), point2.getP()) &&
                Objects.equals(point1.getQmaxP(), point2.getQmaxP()) &&
                Objects.equals(point1.getQminP(), point2.getQminP());
    }
}
