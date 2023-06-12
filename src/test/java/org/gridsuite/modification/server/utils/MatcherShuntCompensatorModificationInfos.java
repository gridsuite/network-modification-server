/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.hamcrest.Description;

import java.util.Objects;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class MatcherShuntCompensatorModificationInfos extends MatcherModificationInfos<ShuntCompensatorModificationInfos> {

    protected MatcherShuntCompensatorModificationInfos(ShuntCompensatorModificationInfos ref) {
        super(ref);
    }

    public static MatcherShuntCompensatorModificationInfos createMatcherShuntCompensatorModificationInfos(ShuntCompensatorModificationInfos shuntCompensatorModificationInfos) {
        return new MatcherShuntCompensatorModificationInfos(shuntCompensatorModificationInfos);
    }

    @Override
    public boolean matchesSafely(ShuntCompensatorModificationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(reference.getEquipmentId())
                && Objects.equals(m.getEquipmentName(), reference.getEquipmentName())
                && Objects.equals(m.getSusceptancePerSection(), reference.getSusceptancePerSection())
                && Objects.equals(m.getShuntCompensatorType(), reference.getShuntCompensatorType())
                && Objects.equals(m.getQAtNominalV(), reference.getQAtNominalV());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
