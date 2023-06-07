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

import java.util.List;
import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGeneratorCreationInfos extends MatcherModificationInfos<GeneratorCreationInfos> {
    public MatcherGeneratorCreationInfos(GeneratorCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(GeneratorCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(getReference().getEquipmentId())
            && StringUtils.equals(m.getEquipmentName(), getReference().getEquipmentName())
            && m.getVoltageLevelId().equals(getReference().getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(getReference().getBusOrBusbarSectionId())
            && m.getEnergySource() == getReference().getEnergySource()
            && m.getMinActivePower() == getReference().getMinActivePower()
            && m.getMaxActivePower() == getReference().getMaxActivePower()
            && Objects.equals(m.getRatedNominalPower(), getReference().getRatedNominalPower())
            && m.getActivePowerSetpoint() == getReference().getActivePowerSetpoint()
            && Objects.equals(m.getReactivePowerSetpoint(), getReference().getReactivePowerSetpoint())
            && m.isVoltageRegulationOn() == getReference().isVoltageRegulationOn()
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
            && matchesReactiveCapabilityCurvePoints(m.getReactiveCapabilityCurvePoints(), getReference().getReactiveCapabilityCurvePoints());
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
