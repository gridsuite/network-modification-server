/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.hamcrest.Description;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
public class MatcherLoadModificationInfos extends MatcherModificationInfos<LoadModificationInfos> {

    public static MatcherLoadModificationInfos createMatcherLoadModificationInfos(LoadModificationInfos loadModificationInfos) {
        return new MatcherLoadModificationInfos(loadModificationInfos);
    }

    protected MatcherLoadModificationInfos(LoadModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LoadModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && (m.getEquipmentName() == null && reference.getEquipmentName() == null || m.getEquipmentName().equals(reference.getEquipmentName()))
                && (m.getVoltageLevelId() == null && reference.getVoltageLevelId() == null || m.getVoltageLevelId().equals(reference.getVoltageLevelId()))
                && (m.getBusOrBusbarSectionId() == null && reference.getBusOrBusbarSectionId() == null || m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId()))
                && (m.getLoadType() == null && reference.getLoadType() == null || m.getLoadType().equals(reference.getLoadType()))
                && (m.getActivePower() == null && reference.getActivePower() == null || m.getActivePower().equals(reference.getActivePower()))
                && (m.getReactivePower() == null && reference.getReactivePower() == null || m.getReactivePower().equals(reference.getReactivePower()));
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
