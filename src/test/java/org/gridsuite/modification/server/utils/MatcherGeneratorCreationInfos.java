/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.hamcrest.Description;

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
            && ((m.getRatedNominalPower() != null && m.getRatedNominalPower().equals(reference.getRatedNominalPower()))
            || (m.getRatedNominalPower() == null && reference.getRatedNominalPower() == null))
            && m.getActivePowerSetpoint() == reference.getActivePowerSetpoint()
            && ((m.getReactivePowerSetpoint() != null && m.getReactivePowerSetpoint().equals(reference.getReactivePowerSetpoint()))
            || (m.getReactivePowerSetpoint() == null && reference.getReactivePowerSetpoint() == null))
            && m.isVoltageRegulationOn() == reference.isVoltageRegulationOn()
            && ((m.getVoltageSetpoint() != null && m.getVoltageSetpoint().equals(reference.getVoltageSetpoint()))
            || (m.getVoltageSetpoint() == null && reference.getVoltageSetpoint() == null));
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
