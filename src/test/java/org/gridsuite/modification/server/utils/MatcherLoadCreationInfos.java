/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.hamcrest.Description;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherLoadCreationInfos extends MatcherModificationInfos<LoadCreationInfos> {

    public static MatcherLoadCreationInfos createMatcherLoadCreationInfos(LoadCreationInfos loadCreationInfos) {
        return new MatcherLoadCreationInfos(loadCreationInfos);
    }

    protected MatcherLoadCreationInfos(LoadCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LoadCreationInfos m) {
        return super.matchesSafely(m)
            && m.getId().equals(reference.getId())
            && m.getName().equals(reference.getName())
            && m.getVoltageLevelId().equals(reference.getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId())
            && m.getLoadType() == reference.getLoadType()
            && m.getP0() == reference.getP0()
            && m.getQ0() == reference.getQ0();
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
