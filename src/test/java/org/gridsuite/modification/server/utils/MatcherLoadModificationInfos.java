/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LoadModificationInfos;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
public class MatcherLoadModificationInfos extends MatcherModificationInfos<LoadModificationInfos> {
    public MatcherLoadModificationInfos(LoadModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LoadModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(getReference().getEquipmentId())
                && ((m.getEquipmentName() == null && getReference().getEquipmentName() == null) || m.getEquipmentName().equals(getReference().getEquipmentName()))
                && ((m.getVoltageLevelId() == null && getReference().getVoltageLevelId() == null) || m.getVoltageLevelId().equals(getReference().getVoltageLevelId()))
                && ((m.getBusOrBusbarSectionId() == null && getReference().getBusOrBusbarSectionId() == null) || m.getBusOrBusbarSectionId().equals(getReference().getBusOrBusbarSectionId()))
                && ((m.getLoadType() == null && getReference().getLoadType() == null) || m.getLoadType().equals(getReference().getLoadType()))
                && ((m.getActivePower() == null && getReference().getActivePower() == null) || m.getActivePower().equals(getReference().getActivePower()))
                && ((m.getReactivePower() == null && getReference().getReactivePower() == null) || m.getReactivePower().equals(getReference().getReactivePower()));
    }
}
