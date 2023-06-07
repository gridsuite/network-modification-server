/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LoadCreationInfos;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherLoadCreationInfos extends MatcherModificationInfos<LoadCreationInfos> {
    public MatcherLoadCreationInfos(LoadCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LoadCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(getReference().getEquipmentId())
            && m.getEquipmentName().equals(getReference().getEquipmentName())
            && m.getVoltageLevelId().equals(getReference().getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(getReference().getBusOrBusbarSectionId())
            && m.getLoadType() == getReference().getLoadType()
            && m.getActivePower() == getReference().getActivePower()
            && m.getReactivePower() == getReference().getReactivePower();
    }
}
