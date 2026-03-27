package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface CompositeModificationRepository extends JpaRepository<CompositeModificationEntity, UUID> {
}
