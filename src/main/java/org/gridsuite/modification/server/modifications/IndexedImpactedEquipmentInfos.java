package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.dto.elasticsearch.BasicEquipmentInfos;

public record IndexedImpactedEquipmentInfos<T extends BasicEquipmentInfos>(
    T impactedEquipmentInfos,
    boolean shouldIndexImpactedEquipment,
    boolean shouldIndexInModification
) { }
