package org.gridsuite.modification.server.modifications;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfosToDelete;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

@NoArgsConstructor
@Getter
public class ImpactedEquipmentsInfos {
    private final List<EquipmentInfosToDelete> deletedEquipments = new ArrayList<>();
    private final List<EquipmentInfos> createdEquipments = new ArrayList<>();
    private final List<EquipmentInfos> modifiedEquipments = new ArrayList<>();

    public List<String> getAllEquipmentsIds() {
        return Stream.concat(
            Stream.concat(createdEquipments.stream(), modifiedEquipments.stream()).map(EquipmentInfos::getId),
            deletedEquipments.stream().map(EquipmentInfosToDelete::id)
        ).toList();
    }
}
