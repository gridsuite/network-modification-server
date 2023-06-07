/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;

import java.util.List;
import java.util.Objects;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class MatcherGeneratorModificationInfos extends MatcherModificationInfos<GeneratorModificationInfos> {
    public MatcherGeneratorModificationInfos(GeneratorModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(GeneratorModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(getReference().getEquipmentId())
                && Objects.equals(m.getEquipmentName(), getReference().getEquipmentName())
                && ((m.getVoltageLevelId() == null && getReference().getVoltageLevelId() == null) || m.getVoltageLevelId().equals(getReference().getVoltageLevelId()))
                && ((m.getBusOrBusbarSectionId() == null && getReference().getBusOrBusbarSectionId() == null) || m.getBusOrBusbarSectionId().equals(getReference().getBusOrBusbarSectionId()))
                && Objects.equals(m.getEnergySource(), getReference().getEnergySource())
                && Objects.equals(m.getMinActivePower(), getReference().getMinActivePower())
                && Objects.equals(m.getMaxActivePower(), getReference().getMaxActivePower())
                && Objects.equals(m.getRatedNominalPower(), getReference().getRatedNominalPower())
                && Objects.equals(m.getActivePowerSetpoint(), getReference().getActivePowerSetpoint())
                && Objects.equals(m.getReactivePowerSetpoint(), getReference().getReactivePowerSetpoint())
                && Objects.equals(m.getVoltageRegulationOn(), getReference().getVoltageRegulationOn())
                && Objects.equals(m.getVoltageSetpoint(), getReference().getVoltageSetpoint())
                && Objects.equals(m.getDroop(), getReference().getDroop())
                && Objects.equals(m.getParticipate(), getReference().getParticipate())
                && Objects.equals(m.getRegulatingTerminalId(), getReference().getRegulatingTerminalId())
                && Objects.equals(m.getRegulatingTerminalType(), getReference().getRegulatingTerminalType())
                && Objects.equals(m.getRegulatingTerminalVlId(), getReference().getRegulatingTerminalVlId())
                && Objects.equals(m.getMaximumReactivePower(), getReference().getMaximumReactivePower())
                && Objects.equals(m.getMinimumReactivePower(), getReference().getMinimumReactivePower())
                && Objects.equals(m.getPlannedActivePowerSetPoint(), getReference().getPlannedActivePowerSetPoint())
                && Objects.equals(m.getStartupCost(), getReference().getStartupCost())
                && Objects.equals(m.getMarginalCost(), getReference().getMarginalCost())
                && Objects.equals(m.getPlannedOutageRate(), getReference().getPlannedOutageRate())
                && Objects.equals(m.getForcedOutageRate(), getReference().getForcedOutageRate())
                && Objects.equals(m.getStepUpTransformerReactance(), getReference().getStepUpTransformerReactance())
                && Objects.equals(m.getTransientReactance(), getReference().getTransientReactance())
                && Objects.equals(m.getReactiveCapabilityCurve(), getReference().getReactiveCapabilityCurve())
                && Objects.equals(m.getQPercent(), getReference().getQPercent())
                && Objects.equals(m.getVoltageRegulationType(), getReference().getVoltageRegulationType())
                && matchesReactiveCapabilityCurvePoints(m.getReactiveCapabilityCurvePoints(), getReference().getReactiveCapabilityCurvePoints());
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
}
