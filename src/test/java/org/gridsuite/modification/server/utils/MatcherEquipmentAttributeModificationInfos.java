/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class MatcherEquipmentAttributeModificationInfos extends MatcherModificationInfos<EquipmentAttributeModificationInfos> {
    public MatcherEquipmentAttributeModificationInfos(String equipmentId, String equipmentAttributeName,
                                                      Object equipmentAttributeValue, IdentifiableType equipmentType) {
        this(EquipmentAttributeModificationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .equipmentId(equipmentId)
                .equipmentAttributeName(equipmentAttributeName)
                .equipmentAttributeValue(equipmentAttributeValue)
                .equipmentType(equipmentType)
                .build());
    }

    public MatcherEquipmentAttributeModificationInfos(EquipmentAttributeModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(EquipmentAttributeModificationInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(m.getEquipmentId(), getReference().getEquipmentId())
                && Objects.equals(m.getEquipmentType(), getReference().getEquipmentType())
                && Objects.equals(m.getEquipmentAttributeName(), getReference().getEquipmentAttributeName())
                && Objects.equals(m.getEquipmentAttributeValue(), getReference().getEquipmentAttributeValue());
    }
}
