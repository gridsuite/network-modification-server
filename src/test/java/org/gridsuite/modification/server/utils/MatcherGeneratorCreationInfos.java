/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.hamcrest.Description;

import java.util.List;
import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGeneratorCreationInfos extends MatcherModificationInfos<GeneratorCreationInfos> {

    protected MatcherGeneratorCreationInfos(GeneratorCreationInfos ref) {
        super(ref);
    }

    public static MatcherGeneratorCreationInfos createMatcherGeneratorCreationInfos(GeneratorCreationInfos generatorCreationInfos) {
        return new MatcherGeneratorCreationInfos(generatorCreationInfos);
    }

    @Override
    public boolean matchesSafely(GeneratorCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(reference.getEquipmentId())
            && m.getSubstationIds().equals(reference.getSubstationIds())
            && StringUtils.equals(m.getEquipmentName(), reference.getEquipmentName())
            && m.getVoltageLevelId().equals(reference.getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId())
            && m.getEnergySource() == reference.getEnergySource()
            && m.getMinActivePower() == reference.getMinActivePower()
            && m.getMaxActivePower() == reference.getMaxActivePower()
            && Objects.equals(m.getRatedNominalPower(), reference.getRatedNominalPower())
            && m.getActivePowerSetpoint() == reference.getActivePowerSetpoint()
            && Objects.equals(m.getReactivePowerSetpoint(), reference.getReactivePowerSetpoint())
            && m.isVoltageRegulationOn() == reference.isVoltageRegulationOn()
            && Objects.equals(m.getVoltageSetpoint(), reference.getVoltageSetpoint())
            && Objects.equals(m.getDroop(), reference.getDroop())
            && Objects.equals(m.getParticipate(), reference.getParticipate())
            && Objects.equals(m.getRegulatingTerminalId(), reference.getRegulatingTerminalId())
            && Objects.equals(m.getRegulatingTerminalType(), reference.getRegulatingTerminalType())
            && Objects.equals(m.getRegulatingTerminalVlId(), reference.getRegulatingTerminalVlId())
            && Objects.equals(m.getMaximumReactivePower(), reference.getMaximumReactivePower())
            && Objects.equals(m.getMinimumReactivePower(), reference.getMinimumReactivePower())
            && Objects.equals(m.getPlannedActivePowerSetPoint(), reference.getPlannedActivePowerSetPoint())
            && Objects.equals(m.getStartupCost(), reference.getStartupCost())
            && Objects.equals(m.getMarginalCost(), reference.getMarginalCost())
            && Objects.equals(m.getPlannedOutageRate(), reference.getPlannedOutageRate())
            && Objects.equals(m.getForcedOutageRate(), reference.getForcedOutageRate())
            && Objects.equals(m.getStepUpTransformerReactance(), reference.getStepUpTransformerReactance())
            && Objects.equals(m.getTransientReactance(), reference.getTransientReactance())
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
        return (point1 == null && point2 == null) ||
               (point1 != null && point2 != null &&
                Objects.equals(point1.getP(), point2.getP()) &&
                Objects.equals(point1.getQmaxP(), point2.getQmaxP()) &&
                Objects.equals(point1.getQminP(), point2.getQminP()));
    }
}
