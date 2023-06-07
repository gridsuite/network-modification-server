/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@AllArgsConstructor
public class MatcherModificationInfos<T extends ModificationInfos> extends TypeSafeMatcher<T> {
    @Getter(AccessLevel.PROTECTED)
    private T reference;

    @Override
    public boolean matchesSafely(T m) {
        return m.getClass().equals(reference.getClass());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
