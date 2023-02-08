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
                && m.getId().equals(reference.getId())
                && ((m.getName() == null && reference.getName() == null) || m.getName().equals(reference.getName()))
                && ((m.getVoltageLevelId() == null && reference.getVoltageLevelId() == null) || m.getVoltageLevelId().equals(reference.getVoltageLevelId()))
                && ((m.getBusOrBusbarSectionId() == null && reference.getBusOrBusbarSectionId() == null) || m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId()))
                && ((m.getLoadType() == null && reference.getLoadType() == null) || m.getLoadType().equals(reference.getLoadType()))
                && ((m.getP0() == null && reference.getP0() == null) || m.getP0().equals(reference.getP0()))
                && ((m.getQ0() == null && reference.getQ0() == null) || m.getQ0().equals(reference.getQ0()));
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
