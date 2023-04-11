/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.hamcrest.Description;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherEquipmentDeletionInfos extends MatcherModificationInfos<EquipmentDeletionInfos> {

    public static MatcherEquipmentDeletionInfos createMatcherEquipmentDeletionInfos(EquipmentDeletionInfos deletionInfos) {
        return new MatcherEquipmentDeletionInfos(deletionInfos);
    }

    protected MatcherEquipmentDeletionInfos(EquipmentDeletionInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(EquipmentDeletionInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
            && m.getEquipmentType().equals(reference.getEquipmentType());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
