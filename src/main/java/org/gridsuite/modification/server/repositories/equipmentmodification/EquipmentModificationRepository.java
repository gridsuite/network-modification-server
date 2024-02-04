package org.gridsuite.modification.server.repositories.equipmentmodification;

import java.util.List;
import java.util.UUID;

public interface EquipmentModificationRepository {
    void deleteSubModificationsByIds(List<UUID> subModificationIds, UUID tabularModificationId);
}
