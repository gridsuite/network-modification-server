/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */
public class MatcherTwoWindingsTransformerCreationInfos extends MatcherModificationInfos<TwoWindingsTransformerCreationInfos> {

    public static MatcherTwoWindingsTransformerCreationInfos createMatcherTwoWindingsTransformerCreationInfos(
            String equipmentId,
            String equipmentName,
            Set<String> substationIds,
            String voltageLevelId1,
            String busOrBusbarSectionId1,
            String voltageLevelId2,
            String busOrBusbarSectionId2,
            double ratedVoltage1,
            double ratedVoltage2,
            double magnetizingSusceptance,
            double magnetizingConductance,
            double seriesReactance,
            double seriesResistance) {
        return new MatcherTwoWindingsTransformerCreationInfos(TwoWindingsTransformerCreationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId(equipmentId)
                .substationIds(substationIds)
                .equipmentName(equipmentName)
                .voltageLevelId1(voltageLevelId1)
                .busOrBusbarSectionId1(busOrBusbarSectionId1)
                .voltageLevelId2(voltageLevelId2)
                .busOrBusbarSectionId2(busOrBusbarSectionId2)
                .ratedVoltage1(ratedVoltage1)
                .ratedVoltage2(ratedVoltage2)
                .magnetizingSusceptance(magnetizingSusceptance)
                .magnetizingConductance(magnetizingConductance)
                .seriesReactance(seriesReactance)
                .seriesResistance(seriesResistance)
                .build());
    }

    public static MatcherTwoWindingsTransformerCreationInfos createMatcherTwoWindingsTransformerCreationInfos(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        return new MatcherTwoWindingsTransformerCreationInfos(twoWindingsTransformerCreationInfos);
    }

    protected MatcherTwoWindingsTransformerCreationInfos(TwoWindingsTransformerCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(TwoWindingsTransformerCreationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && m.getSubstationIds().equals(reference.getSubstationIds())
                && m.getEquipmentName().equals(reference.getEquipmentName())
                && m.getVoltageLevelId1().equals(reference.getVoltageLevelId1())
                && m.getVoltageLevelId2().equals(reference.getVoltageLevelId2())
                && m.getBusOrBusbarSectionId1().equals(reference.getBusOrBusbarSectionId1())
                && m.getBusOrBusbarSectionId2().equals(reference.getBusOrBusbarSectionId2())
                && m.getRatedVoltage1() == reference.getRatedVoltage1()
                && m.getRatedVoltage2() == reference.getRatedVoltage2()
                && m.getMagnetizingSusceptance() == reference.getMagnetizingSusceptance()
                && m.getMagnetizingConductance() == reference.getMagnetizingConductance()
                && m.getSeriesReactance() == reference.getSeriesReactance()
                && m.getSeriesResistance() == reference.getSeriesResistance();
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
