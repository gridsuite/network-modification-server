/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.LoadType;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherLoadCreationInfos extends MatcherModificationInfos<LoadCreationInfos> {

    public static MatcherLoadCreationInfos createMatcherLoadCreationInfos(String equipmentId,
                                                                               String equipmentName,
                                                                               Set<String> substationIds,
                                                                               String voltageLevelId,
                                                                               String busOrBusbarSectionId,
                                                                          LoadType loadType,
                                                                          double activePower,
                                                                          double reactivePower) {
        return new MatcherLoadCreationInfos(LoadCreationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(ModificationType.LOAD_CREATION)
                .equipmentId(equipmentId)
                .substationIds(substationIds)
                .equipmentName(equipmentName)
                .voltageLevelId(voltageLevelId)
            .busOrBusbarSectionId(busOrBusbarSectionId)
            .loadType(loadType)
            .activePower(activePower)
                .reactivePower(reactivePower)
                .build());
    }

    public static MatcherLoadCreationInfos createMatcherLoadCreationInfos(LoadCreationInfos loadCreationInfos) {
        return new MatcherLoadCreationInfos(loadCreationInfos);
    }

    protected MatcherLoadCreationInfos(LoadCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LoadCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(reference.getEquipmentId())
            && m.getSubstationIds().equals(reference.getSubstationIds())
            && m.getEquipmentName().equals(reference.getEquipmentName())
            && m.getVoltageLevelId().equals(reference.getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId())
            && m.getLoadType() == reference.getLoadType()
            && m.getActivePower() == reference.getActivePower()
            && m.getReactivePower() == reference.getReactivePower();
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
