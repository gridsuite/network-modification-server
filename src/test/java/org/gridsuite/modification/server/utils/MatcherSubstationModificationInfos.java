/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.SubstationFreePropertyInfos;
import org.gridsuite.modification.server.dto.SubstationModificationInfos;
import org.hamcrest.Description;

import java.util.List;
import java.util.Objects;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class MatcherSubstationModificationInfos extends MatcherModificationInfos<SubstationModificationInfos> {

    public static MatcherSubstationModificationInfos createMatcherSubstationModificationInfos(SubstationModificationInfos infos) {
        return new MatcherSubstationModificationInfos(infos);
    }

    protected MatcherSubstationModificationInfos(SubstationModificationInfos ref) {
        super(ref);
    }

    private boolean matchesProperties(List<SubstationFreePropertyInfos> l1, List<SubstationFreePropertyInfos> l2) {
        return l1 == null && l2 == null
                || l1 == null && l2.isEmpty()
                || l2 == null && l1.isEmpty()
                || l1 != null && l2 != null && l1.size() == l2.size() && l1.equals(l2);
    }

    @Override
    public boolean matchesSafely(SubstationModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && Objects.equals(m.getEquipmentName(), reference.getEquipmentName())
                && Objects.equals(m.getSubstationCountry(), reference.getSubstationCountry())
                && matchesProperties(m.getProperties(), reference.getProperties());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
