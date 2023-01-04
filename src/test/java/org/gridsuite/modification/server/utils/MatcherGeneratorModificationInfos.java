/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.hamcrest.Description;

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
            && Objects.equals(m.getEnergySource(), reference.getEnergySource())
            && Objects.equals(m.getMinActivePower(), reference.getMinActivePower())
            && Objects.equals(m.getMaxActivePower(), reference.getMaxActivePower())
            && Objects.equals(m.getRatedNominalPower(), reference.getRatedNominalPower())
            && Objects.equals(m.getActivePowerSetpoint(), reference.getActivePowerSetpoint())
            && Objects.equals(m.getReactivePowerSetpoint(), reference.getReactivePowerSetpoint())
            && Objects.equals(m.getVoltageRegulationOn(), reference.getVoltageRegulationOn())
            && Objects.equals(m.getVoltageSetpoint(), reference.getVoltageSetpoint());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
