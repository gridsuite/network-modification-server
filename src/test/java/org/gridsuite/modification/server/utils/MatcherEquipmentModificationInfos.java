/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherEquipmentModificationInfos<T extends EquipmentModificationInfos> extends MatcherModificationInfos<T> {

    public static MatcherEquipmentModificationInfos<EquipmentModificationInfos> createMatcherEquipmentModificationInfos(ModificationType modificationType, String equipmentId, Set<String> substationIds) {
        return new MatcherEquipmentModificationInfos<>(EquipmentModificationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(modificationType)
                .equipmentId(equipmentId)
                .substationIds(substationIds)
                .build());
    }

    public static MatcherEquipmentModificationInfos<EquipmentModificationInfos> createMatcherEquipmentModificationInfos(EquipmentModificationInfos modificationInfos) {
        return new MatcherEquipmentModificationInfos<>(modificationInfos);
    }

    protected MatcherEquipmentModificationInfos(T ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(T m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId());
    }
}
