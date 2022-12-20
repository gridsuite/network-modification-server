/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class MatcherModificationInfos<T extends ModificationInfos> extends TypeSafeMatcher<T> {
    T reference;

    public static MatcherModificationInfos<ModificationInfos> createMatcherModificationInfos(ModificationType modificationType, Set<String> substationIds) {
        return new MatcherModificationInfos<>(ModificationInfos.builder()
            .date(ZonedDateTime.now(ZoneOffset.UTC))
            .type(modificationType)
            .substationIds(substationIds)
            .build());
    }

    protected MatcherModificationInfos(T ref) {
        this.reference = ref;
    }

    @Override
    public boolean matchesSafely(T m) {
        return m.getType() == reference.getType()
            && m.getSubstationIds().equals(reference.getSubstationIds())
            && m.getDate().toEpochSecond() - reference.getDate().toEpochSecond() < 2;
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
