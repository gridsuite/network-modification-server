/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.ModificationInfos;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class MatcherModificationInfos<T extends ModificationInfos> extends TypeSafeMatcher<T> {
    T reference;

    protected MatcherModificationInfos(T ref) {
        this.reference = ref;
    }

    @Override
    public boolean matchesSafely(T m) {
        return m.getClass().equals(reference.getClass())
        // TODO we have to set the substations into the input DTO, maybe there is a better way ? Waiting for the new result output class
            && m.getSubstationIds().equals(reference.getSubstationIds());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
