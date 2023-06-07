/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;

import java.util.Objects;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
public class MatcherLineSplitWithVoltageLevelInfos extends MatcherModificationInfos<LineSplitWithVoltageLevelInfos> {
    public MatcherLineSplitWithVoltageLevelInfos(LineSplitWithVoltageLevelInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LineSplitWithVoltageLevelInfos m) {
        return super.matchesSafely(m)
            && Math.abs(getReference().getPercent() - m.getPercent()) < 0.2
            && Objects.equals(getReference().getLineToSplitId(), m.getLineToSplitId())
            && (getReference().getMayNewVoltageLevelInfos() == null && m.getMayNewVoltageLevelInfos() == null
              || new MatcherVoltageLevelCreationInfos(getReference().getMayNewVoltageLevelInfos()).matchesSafely(m.getMayNewVoltageLevelInfos()))
            && Objects.equals(getReference().getExistingVoltageLevelId(), m.getExistingVoltageLevelId())
            && Objects.equals(getReference().getBbsOrBusId(), m.getBbsOrBusId())
            && Objects.equals(getReference().getNewLine1Id(), m.getNewLine1Id())
            && Objects.equals(getReference().getNewLine1Name(), m.getNewLine1Name())
            && Objects.equals(getReference().getNewLine2Id(), m.getNewLine2Id())
            && Objects.equals(getReference().getNewLine2Name(), m.getNewLine2Name());
    }
}
