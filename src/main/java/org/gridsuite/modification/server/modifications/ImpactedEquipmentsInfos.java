package org.gridsuite.modification.server.modifications;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfosToDelete;

import java.util.ArrayList;
import java.util.List;

@NoArgsConstructor
@Getter
public class ImpactedEquipmentsInfos {
    private final List<EquipmentInfosToDelete> deletedEquipments = new ArrayList<>();

    private final List<EquipmentInfos> createdEquipments = new ArrayList<>();

    private final List<EquipmentInfos> modifiedEquipments = new ArrayList<>();

    public List<String> getAllEquipmentIds() {
        List<String> ids = new ArrayList<>();
        ids.addAll(deletedEquipments.stream().map(EquipmentInfosToDelete::id).toList());
        ids.addAll(createdEquipments.stream().map(EquipmentInfos::getId).toList());
        ids.addAll(modifiedEquipments.stream().map(EquipmentInfos::getId).toList());
        return ids;
    }
}
