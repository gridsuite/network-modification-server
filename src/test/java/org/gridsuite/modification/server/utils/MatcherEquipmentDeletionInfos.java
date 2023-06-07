/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;

import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherEquipmentDeletionInfos extends MatcherModificationInfos<EquipmentDeletionInfos> {
    public MatcherEquipmentDeletionInfos(EquipmentDeletionInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(EquipmentDeletionInfos m) {
        return super.matchesSafely(m)
            && Objects.equals(m.getEquipmentId(), getReference().getEquipmentId())
            && Objects.equals(m.getEquipmentType(), getReference().getEquipmentType());
    }
}
