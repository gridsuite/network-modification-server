/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class MatcherElementaryModificationInfos extends MatcherModificationInfos<ElementaryModificationInfos> {

    public static MatcherElementaryModificationInfos createMatcherElementaryModificationInfos(String equipmentId, String equipmentName,
                                                                                       String equipmentAttributeName, String equipmentAttributeValue) {
        return new MatcherElementaryModificationInfos(ElementaryModificationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .equipmentId(equipmentId).equipmentName(equipmentName)
                .equipmentAttributeName(equipmentAttributeName).equipmentAttributeValue(equipmentAttributeValue)
                .build());
    }

    public static MatcherElementaryModificationInfos createMatcherElementaryModificationInfos(ElementaryModificationInfos modificationInfos) {
        return new MatcherElementaryModificationInfos(modificationInfos);
    }

    protected MatcherElementaryModificationInfos(ElementaryModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(ElementaryModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && m.getEquipmentName().equals(reference.getEquipmentName())
                && m.getEquipmentAttributeName().equals(reference.getEquipmentAttributeName())
                && m.getEquipmentAttributeValue().equals(reference.getEquipmentAttributeValue());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
