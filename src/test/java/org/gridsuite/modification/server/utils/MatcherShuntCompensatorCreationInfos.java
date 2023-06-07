/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;

import java.util.Objects;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */
public class MatcherShuntCompensatorCreationInfos extends MatcherModificationInfos<ShuntCompensatorCreationInfos> {
    public MatcherShuntCompensatorCreationInfos(ShuntCompensatorCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(ShuntCompensatorCreationInfos m) {
        return super.matchesSafely(m)
            && Objects.equals(m.getEquipmentId(), getReference().getEquipmentId())
            && StringUtils.equals(m.getEquipmentName(), getReference().getEquipmentName())
            && Objects.equals(m.getVoltageLevelId(), getReference().getVoltageLevelId())
            && Objects.equals(m.getBusOrBusbarSectionId(), getReference().getBusOrBusbarSectionId())
            && Objects.equals(m.getIsIdenticalSection(), getReference().getIsIdenticalSection())
            && Objects.equals(m.getSusceptancePerSection(), getReference().getSusceptancePerSection())
            && Objects.equals(m.getMaximumNumberOfSections(), getReference().getMaximumNumberOfSections())
            && Objects.equals(m.getCurrentNumberOfSections(), getReference().getCurrentNumberOfSections())
            && Objects.equals(m.getShuntCompensatorType(), getReference().getShuntCompensatorType())
            && Objects.equals(m.getQAtNominalV(), getReference().getQAtNominalV());
    }
}
