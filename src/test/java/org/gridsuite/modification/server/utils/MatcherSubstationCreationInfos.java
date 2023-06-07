/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.SubstationCreationInfos;

import java.util.Objects;

/**
 * @author Abdelsalem HEDHILI <abdelsalem.hedhili at rte-france.com>
 */
public class MatcherSubstationCreationInfos extends MatcherModificationInfos<SubstationCreationInfos> {
    public MatcherSubstationCreationInfos(SubstationCreationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(SubstationCreationInfos m) {
        return super.matchesSafely(m)
                && m.getEquipmentId().equals(getReference().getEquipmentId())
                && m.getEquipmentName().equals(getReference().getEquipmentName())
                && m.getSubstationCountry().equals(getReference().getSubstationCountry())
                && Objects.equals(m.getProperties(), getReference().getProperties());
    }
}
