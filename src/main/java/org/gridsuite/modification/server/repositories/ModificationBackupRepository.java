package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.ModificationBackupEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.UUID;

public interface ModificationBackupRepository extends JpaRepository<ModificationBackupEntity, UUID> {
    void deleteAllByNetworkUuidAndModificationIdIn(UUID networkUuid, List<UUID> modificationIds);

    void deleteAllByModificationIdIn(List<UUID> modificationIds);
}
