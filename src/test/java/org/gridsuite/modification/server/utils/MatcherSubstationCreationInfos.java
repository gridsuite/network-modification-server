/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.Country;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.SubstationCreationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;

/**
 * @author Abdelsalem HEDHILI <abdelsalem.hedhili at rte-france.com>
 */
public class MatcherSubstationCreationInfos extends MatcherModificationInfos<SubstationCreationInfos> {

    public static MatcherSubstationCreationInfos createMatcherSubstationCreationInfos(String equipmentId,
                                                                          String equipmentName,
                                                                          Country country) {
        return new MatcherSubstationCreationInfos(SubstationCreationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(ModificationType.LOAD_CREATION)
                .equipmentId(equipmentId)
                .equipmentName(equipmentName)
                .substationCountry(country)
                .build());
    }

    public static MatcherSubstationCreationInfos createMatcherSubstationCreationInfos(SubstationCreationInfos substationCreationInfos) {
        return new MatcherSubstationCreationInfos(substationCreationInfos);
    }

    protected MatcherSubstationCreationInfos(SubstationCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(SubstationCreationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && m.getEquipmentName().equals(reference.getEquipmentName())
                && m.getSubstationCountry().equals(reference.getSubstationCountry());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
