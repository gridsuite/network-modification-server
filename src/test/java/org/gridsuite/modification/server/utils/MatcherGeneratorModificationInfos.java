/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.hamcrest.Description;

import java.util.List;
import java.util.Objects;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class MatcherGeneratorModificationInfos extends MatcherModificationInfos<GeneratorModificationInfos> {

    protected MatcherGeneratorModificationInfos(GeneratorModificationInfos ref) {
        super(ref);
    }

    public static MatcherGeneratorModificationInfos createMatcherGeneratorModificationInfos(GeneratorModificationInfos generatorModificationInfos) {
        return new MatcherGeneratorModificationInfos(generatorModificationInfos);
    }

    @Override
    public boolean matchesSafely(GeneratorModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && m.getSubstationIds().equals(reference.getSubstationIds())
                && m.getEquipmentName().equals(reference.getEquipmentName())
                && ((m.getVoltageLevelId() == null && reference.getVoltageLevelId() == null) || m.getVoltageLevelId().equals(reference.getVoltageLevelId()))
                && ((m.getBusOrBusbarSectionId() == null && reference.getBusOrBusbarSectionId() == null) || m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId()))
                && Objects.equals(m.getEnergySource(), reference.getEnergySource())
                && Objects.equals(m.getMinActivePower(), reference.getMinActivePower())
                && Objects.equals(m.getMaxActivePower(), reference.getMaxActivePower())
                && Objects.equals(m.getRatedNominalPower(), reference.getRatedNominalPower())
                && Objects.equals(m.getActivePowerSetpoint(), reference.getActivePowerSetpoint())
                && Objects.equals(m.getReactivePowerSetpoint(), reference.getReactivePowerSetpoint())
                && Objects.equals(m.getVoltageRegulationOn(), reference.getVoltageRegulationOn())
                && Objects.equals(m.getVoltageSetpoint(), reference.getVoltageSetpoint())
                && Objects.equals(m.getDroop(), reference.getDroop())
                && Objects.equals(m.getParticipate(), reference.getParticipate())
                && Objects.equals(m.getRegulatingTerminalId(), reference.getRegulatingTerminalId())
                && Objects.equals(m.getRegulatingTerminalType(), reference.getRegulatingTerminalType())
                && Objects.equals(m.getRegulatingTerminalVlId(), reference.getRegulatingTerminalVlId())
                && Objects.equals(m.getMaximumReactivePower(), reference.getMaximumReactivePower())
                && Objects.equals(m.getMinimumReactivePower(), reference.getMinimumReactivePower())
                && Objects.equals(m.getMarginalCost(), reference.getMarginalCost())
                && Objects.equals(m.getStepUpTransformerReactance(), reference.getStepUpTransformerReactance())
                && Objects.equals(m.getTransientReactance(), reference.getTransientReactance())
                && Objects.equals(m.getReactiveCapabilityCurve(), reference.getReactiveCapabilityCurve())
                && Objects.equals(m.getQPercent(), reference.getQPercent())
                && Objects.equals(m.getVoltageRegulationType(), reference.getVoltageRegulationType())
                && matchesReactiveCapabilityCurvePoints(m.getReactiveCapabilityCurvePoints(), reference.getReactiveCapabilityCurvePoints());
    }

    private static boolean matchesReactiveCapabilityCurvePoints(List<ReactiveCapabilityCurveModificationInfos> points1, List<ReactiveCapabilityCurveModificationInfos> points2) {
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

    private static boolean matchesReactiveCapabilityCurve(ReactiveCapabilityCurveModificationInfos point1, ReactiveCapabilityCurveModificationInfos point2) {
        return (point1 == null && point2 == null) ||
                (point1 != null && point2 != null &&
                        Objects.equals(point1.getP(), point2.getP()) &&
                        Objects.equals(point1.getQmaxP(), point2.getQmaxP()) &&
                        Objects.equals(point1.getQminP(), point2.getQminP())) &&
                        Objects.equals(point1.getOldP(), point2.getOldP()) &&
                        Objects.equals(point1.getOldQmaxP(), point2.getOldQmaxP()) &&
                        Objects.equals(point1.getOldQminP(), point2.getOldQminP());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
