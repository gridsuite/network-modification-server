/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherEquipmentModificationInfos extends MatcherModificationInfos<EquipmenModificationInfos> {

    public static MatcherEquipmentModificationInfos createMatcherEquipmentModificationInfos(ModificationType modificationType, String equipmentId, Set<String> substationIds) {
        return new MatcherEquipmentModificationInfos(EquipmenModificationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(modificationType)
                .equipmentId(equipmentId)
                .substationIds(substationIds)
                .build());
    }

    public static MatcherEquipmentModificationInfos createMatcherEquipmentModificationInfos(EquipmenModificationInfos modificationInfos) {
        return new MatcherEquipmentModificationInfos(modificationInfos);
    }

    protected MatcherEquipmentModificationInfos(EquipmenModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(EquipmenModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && m.getSubstationIds().equals(reference.getSubstationIds());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
