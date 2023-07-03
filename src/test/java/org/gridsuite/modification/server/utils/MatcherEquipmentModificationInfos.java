/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.EquipmentModificationInfos;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherEquipmentModificationInfos<T extends EquipmentModificationInfos> extends MatcherModificationInfos<T> {

    protected MatcherEquipmentModificationInfos(T ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(T m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId());
    }
}