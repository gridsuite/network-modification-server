/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@NoArgsConstructor
@Getter
public class ImpactedEquipmentsInfos {
    private final List<IndexedImpactedEquipmentInfos<TombstonedEquipmentInfos>> tombstonedEquipments = new ArrayList<>();
    private final List<IndexedImpactedEquipmentInfos<EquipmentInfos>> createdEquipments = new ArrayList<>();
    private final List<IndexedImpactedEquipmentInfos<EquipmentInfos>> modifiedEquipments = new ArrayList<>();

    public boolean hasAnyImpactedEquipmentToIndexInModification() {
        return createdEquipments.stream().anyMatch(IndexedImpactedEquipmentInfos::shouldIndexInModification)
            || modifiedEquipments.stream().anyMatch(IndexedImpactedEquipmentInfos::shouldIndexInModification)
            || tombstonedEquipments.stream().anyMatch(IndexedImpactedEquipmentInfos::shouldIndexInModification);
    }
}
