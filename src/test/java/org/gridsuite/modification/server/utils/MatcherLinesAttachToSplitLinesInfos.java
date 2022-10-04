/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LinesAttachToSplitLinesInfos;

import java.util.Objects;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class MatcherLinesAttachToSplitLinesInfos extends MatcherModificationInfos<LinesAttachToSplitLinesInfos> {
    protected MatcherLinesAttachToSplitLinesInfos(LinesAttachToSplitLinesInfos ref) {
        super(ref);
    }

    public static MatcherLinesAttachToSplitLinesInfos createMatcherLinesAttachToSplitLinesInfos(LinesAttachToSplitLinesInfos toLinesAttachToSplitLinesInfos) {
        return new MatcherLinesAttachToSplitLinesInfos(toLinesAttachToSplitLinesInfos);
    }

    public boolean matchesSafely(LinesAttachToSplitLinesInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getLineToAttachTo1Id(), m.getLineToAttachTo1Id())
                && Objects.equals(reference.getLineToAttachTo2Id(), m.getLineToAttachTo2Id())
                && Objects.equals(reference.getAttachedLineId(), m.getAttachedLineId())
                && Objects.equals(reference.getVoltageLevelId(), m.getVoltageLevelId())
                && Objects.equals(reference.getBbsBusId(), m.getBbsBusId())
                && Objects.equals(reference.getReplacingLine1Id(), m.getReplacingLine1Id())
                && Objects.equals(reference.getReplacingLine1Name(), m.getReplacingLine1Name())
                && Objects.equals(reference.getReplacingLine2Id(), m.getReplacingLine2Id())
                && Objects.equals(reference.getReplacingLine2Name(), m.getReplacingLine2Name());
    }
}

