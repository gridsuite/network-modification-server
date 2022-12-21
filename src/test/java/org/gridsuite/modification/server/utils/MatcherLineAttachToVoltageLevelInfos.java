/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import java.util.Objects;

import org.gridsuite.modification.server.dto.LineAttachToVoltageLevelInfos;

/**
 * @author Nicolas NOIR <nicolas.noir at rte-france.com>
 */
public class MatcherLineAttachToVoltageLevelInfos extends MatcherModificationInfos<LineAttachToVoltageLevelInfos> {
    protected MatcherLineAttachToVoltageLevelInfos(LineAttachToVoltageLevelInfos ref) {
        super(ref);
    }

    public static MatcherLineAttachToVoltageLevelInfos createMatcherLineAttachToVoltageLevelInfos(LineAttachToVoltageLevelInfos toLineAttachToVoltageLevelInfos) {
        return new MatcherLineAttachToVoltageLevelInfos(toLineAttachToVoltageLevelInfos);
    }

    public boolean matchesSafely(LineAttachToVoltageLevelInfos m) {
        return super.matchesSafely(m)
                && Math.abs(reference.getPercent() - m.getPercent()) < 0.2
                && Objects.equals(reference.getLineToAttachToId(), m.getLineToAttachToId())
                && Objects.equals(reference.getAttachmentPointId(), m.getAttachmentPointId())
                && Objects.equals(reference.getAttachmentPointName(), m.getAttachmentPointName())
                && ((reference.getMayNewVoltageLevelInfos() == null && m.getMayNewVoltageLevelInfos() == null)
                || (reference.getMayNewVoltageLevelInfos() != null && m.getMayNewVoltageLevelInfos() != null && new MatcherVoltageLevelCreationInfos(reference.getMayNewVoltageLevelInfos()).matchesSafely(m.getMayNewVoltageLevelInfos())))
                && Objects.equals(reference.getExistingVoltageLevelId(), m.getExistingVoltageLevelId())
                && Objects.equals(reference.getBbsOrBusId(), m.getBbsOrBusId())
                && ((reference.getAttachmentLine() == null && m.getAttachmentLine() == null)
                || new MatcherLineCreationInfos(reference.getAttachmentLine()).matchesSafely(m.getAttachmentLine()))
                && Objects.equals(reference.getNewLine1Id(), m.getNewLine1Id())
                && Objects.equals(reference.getNewLine1Name(), m.getNewLine1Name())
                && Objects.equals(reference.getNewLine2Id(), m.getNewLine2Id())
                && Objects.equals(reference.getNewLine2Name(), m.getNewLine2Name());
    }
}
