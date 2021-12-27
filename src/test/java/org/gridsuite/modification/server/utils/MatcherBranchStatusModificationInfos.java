/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class MatcherBranchStatusModificationInfos extends MatcherModificationInfos<BranchStatusModificationInfos> {

    protected MatcherBranchStatusModificationInfos(BranchStatusModificationInfos ref) {
        super(ref);
    }

    public static MatcherBranchStatusModificationInfos createMatcherBranchStatusModificationInfos(BranchStatusModificationInfos.ActionType action,
                                                                                                  Set<String> substationIds) {
        return new MatcherBranchStatusModificationInfos(BranchStatusModificationInfos.builder()
            .date(ZonedDateTime.now(ZoneOffset.UTC))
            .type(ModificationType.BRANCH_STATUS)
            .substationIds(substationIds)
            .action(action)
            .build());
    }

    public static MatcherBranchStatusModificationInfos createMatcherBranchStatusModificationInfos(BranchStatusModificationInfos modificationInfos) {
        return new MatcherBranchStatusModificationInfos(modificationInfos);
    }

    @Override
    public boolean matchesSafely(BranchStatusModificationInfos m) {
        return super.matchesSafely(m)
            && m.getAction().equals(reference.getAction());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
