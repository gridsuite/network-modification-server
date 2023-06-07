/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.EquipmentModificationInfos;

import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherEquipmentModificationInfos<T extends EquipmentModificationInfos> extends MatcherModificationInfos<T> {
    public MatcherEquipmentModificationInfos(T ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(T m) {
        return super.matchesSafely(m) && Objects.equals(m.getEquipmentId(), getReference().getEquipmentId());
    }
}
