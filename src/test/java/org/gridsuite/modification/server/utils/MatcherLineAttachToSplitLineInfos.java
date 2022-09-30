/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.LineAttachToSplitLineInfos;

import java.util.Objects;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class MatcherLineAttachToSplitLineInfos extends MatcherModificationInfos<LineAttachToSplitLineInfos> {
    protected MatcherLineAttachToSplitLineInfos(LineAttachToSplitLineInfos ref) {
        super(ref);
    }

    public static MatcherLineAttachToSplitLineInfos createMatcherLineAttachToSplitLineInfos(LineAttachToSplitLineInfos toLineAttachToSplitLineInfos) {
        return new MatcherLineAttachToSplitLineInfos(toLineAttachToSplitLineInfos);
    }

    public boolean matchesSafely(LineAttachToSplitLineInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getLineToAttachTo1Id(), m.getLineToAttachTo1Id())
                && Objects.equals(reference.getLineToAttachTo2Id(), m.getLineToAttachTo2Id())
                && Objects.equals(reference.getAttachedLineId(), m.getAttachedLineId())
                && Objects.equals(reference.getExistingVoltageLevelId(), m.getExistingVoltageLevelId())
                && Objects.equals(reference.getBbsOrBusId(), m.getBbsOrBusId())
                && Objects.equals(reference.getNewLine1Id(), m.getNewLine1Id())
                && Objects.equals(reference.getNewLine1Name(), m.getNewLine1Name())
                && Objects.equals(reference.getNewLine2Id(), m.getNewLine2Id())
                && Objects.equals(reference.getNewLine2Name(), m.getNewLine2Name());
    }
}

