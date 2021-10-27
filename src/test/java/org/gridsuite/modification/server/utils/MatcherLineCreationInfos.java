/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
public class MatcherLineCreationInfos extends MatcherModificationInfos<LineCreationInfos> {

    public static MatcherLineCreationInfos createMatcherLineCreationInfos(String equipmentId,
                                                                                String equipmentName,
                                                                                Set<String> substationIds,
                                                                                double seriesResistance,
                                                                                double seriesReactance,
                                                                                Double shuntConductance1,
                                                                                Double shuntSusceptance1,
                                                                                Double shuntConductance2,
                                                                                Double shuntSusceptance2,
                                                                                String voltageLevelId1,
                                                                                String busOrBusbarSectionId1,
                                                                                String voltageLevelId2,
                                                                                String busOrBusbarSectionId2
                                                                          ) {
        return new MatcherLineCreationInfos(LineCreationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(ModificationType.LINE_CREATION)
                .equipmentId(equipmentId)
                .substationIds(substationIds)
                .equipmentName(equipmentName)
                .seriesResistance(seriesResistance)
                .seriesReactance(seriesReactance)
                .shuntConductance1(shuntConductance1)
                .shuntSusceptance1(shuntSusceptance1)
                .shuntConductance2(shuntConductance2)
                .shuntSusceptance2(shuntSusceptance2)
                .voltageLevelId1(voltageLevelId1)
                .busOrBusbarSectionId1(busOrBusbarSectionId1)
                .voltageLevelId2(voltageLevelId2)
                .busOrBusbarSectionId2(busOrBusbarSectionId2)
                .build());
    }

    public static MatcherLineCreationInfos createMatcherLineCreationInfos(LineCreationInfos lineCreationInfos) {
        return new MatcherLineCreationInfos(lineCreationInfos);
    }

    protected MatcherLineCreationInfos(LineCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LineCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(reference.getEquipmentId())
            && m.getSubstationIds().equals(reference.getSubstationIds())
            && m.getEquipmentName().equals(reference.getEquipmentName())
            && m.getSeriesResistance() == reference.getSeriesResistance()
            && m.getSeriesReactance() == reference.getSeriesReactance()
            && m.getShuntConductance1() != null && m.getShuntConductance1().equals(reference.getShuntConductance1())
            || m.getShuntConductance1() == null && reference.getShuntConductance1() == null
            && m.getShuntSusceptance1() != null && m.getShuntSusceptance1().equals(reference.getShuntSusceptance1())
            || m.getShuntSusceptance1() == null && reference.getShuntSusceptance1() == null
            && m.getShuntConductance2() != null && m.getShuntConductance2().equals(reference.getShuntConductance2())
            || m.getShuntConductance2() == null && reference.getShuntConductance2() == null
            && m.getShuntSusceptance2() != null && m.getShuntSusceptance2().equals(reference.getShuntSusceptance2())
            || m.getShuntSusceptance2() == null && reference.getShuntSusceptance2() == null
            && m.getVoltageLevelId1().equals(reference.getVoltageLevelId1())
            && m.getBusOrBusbarSectionId1().equals(reference.getBusOrBusbarSectionId1())
            && m.getVoltageLevelId2().equals(reference.getVoltageLevelId2())
            && m.getBusOrBusbarSectionId2().equals(reference.getBusOrBusbarSectionId2());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
