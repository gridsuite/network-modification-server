/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.hamcrest.Description;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */
public class MatcherShuntCompensatorCreationInfos extends MatcherModificationInfos<ShuntCompensatorCreationInfos> {

    protected MatcherShuntCompensatorCreationInfos(ShuntCompensatorCreationInfos ref) {
        super(ref);
    }

    public static MatcherShuntCompensatorCreationInfos createMatcher(ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        return new MatcherShuntCompensatorCreationInfos(shuntCompensatorCreationInfos);
    }

    @Override
    public boolean matchesSafely(ShuntCompensatorCreationInfos m) {
        return super.matchesSafely(m)
            && m.getEquipmentId().equals(reference.getEquipmentId())
            && StringUtils.equals(m.getEquipmentName(), reference.getEquipmentName())
            && m.getVoltageLevelId().equals(reference.getVoltageLevelId())
            && m.getBusOrBusbarSectionId().equals(reference.getBusOrBusbarSectionId())
            && m.getIsIdenticalSection().equals(reference.getIsIdenticalSection())
            && Objects.equals(m.getSusceptancePerSection(), reference.getSusceptancePerSection())
            && m.getMaximumNumberOfSections().equals(reference.getMaximumNumberOfSections())
            && m.getCurrentNumberOfSections().equals(reference.getCurrentNumberOfSections())
            && Objects.equals(m.getShuntCompensatorType(), reference.getShuntCompensatorType())
            && Objects.equals(m.getQAtNominalV(), reference.getQAtNominalV());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
