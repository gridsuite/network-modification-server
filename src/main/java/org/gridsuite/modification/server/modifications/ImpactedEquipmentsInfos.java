package org.gridsuite.modification.server.modifications;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.elasticsearch.BasicEquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

@NoArgsConstructor
@Getter
public class ImpactedEquipmentsInfos {
    private final List<TombstonedEquipmentInfos> tombstonedEquipments = new ArrayList<>();
    private final List<EquipmentInfos> createdEquipments = new ArrayList<>();
    private final List<EquipmentInfos> modifiedEquipments = new ArrayList<>();

    public List<String> getAllEquipmentsIds() {
        return Stream.concat(Stream.concat(createdEquipments.stream(), modifiedEquipments.stream()), tombstonedEquipments.stream())
            .map(BasicEquipmentInfos::getId)
            .toList();
    }
}
