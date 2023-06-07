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
    public MatcherLinesAttachToSplitLinesInfos(LinesAttachToSplitLinesInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(LinesAttachToSplitLinesInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(getReference().getLineToAttachTo1Id(), m.getLineToAttachTo1Id())
                && Objects.equals(getReference().getLineToAttachTo2Id(), m.getLineToAttachTo2Id())
                && Objects.equals(getReference().getAttachedLineId(), m.getAttachedLineId())
                && Objects.equals(getReference().getVoltageLevelId(), m.getVoltageLevelId())
                && Objects.equals(getReference().getBbsBusId(), m.getBbsBusId())
                && Objects.equals(getReference().getReplacingLine1Id(), m.getReplacingLine1Id())
                && Objects.equals(getReference().getReplacingLine1Name(), m.getReplacingLine1Name())
                && Objects.equals(getReference().getReplacingLine2Id(), m.getReplacingLine2Id())
                && Objects.equals(getReference().getReplacingLine2Name(), m.getReplacingLine2Name());
    }
}

