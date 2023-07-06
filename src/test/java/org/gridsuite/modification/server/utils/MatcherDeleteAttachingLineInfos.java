/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;

import java.util.Objects;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class MatcherDeleteAttachingLineInfos extends MatcherModificationInfos<DeleteAttachingLineInfos> {
    protected MatcherDeleteAttachingLineInfos(DeleteAttachingLineInfos ref) {
        super(ref);
    }

    public static MatcherDeleteAttachingLineInfos createMatcherDeleteAttachingLineInfos(DeleteAttachingLineInfos toDeleteAttachingLineInfos) {
        return new MatcherDeleteAttachingLineInfos(toDeleteAttachingLineInfos);
    }

    public boolean matchesSafely(DeleteAttachingLineInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getLineToAttachTo1Id(), m.getLineToAttachTo1Id())
                && Objects.equals(reference.getLineToAttachTo2Id(), m.getLineToAttachTo2Id())
                && Objects.equals(reference.getAttachedLineId(), m.getAttachedLineId())
                && Objects.equals(reference.getReplacingLine1Id(), m.getReplacingLine1Id())
                && Objects.equals(reference.getReplacingLine1Name(), m.getReplacingLine1Name());
    }
}
