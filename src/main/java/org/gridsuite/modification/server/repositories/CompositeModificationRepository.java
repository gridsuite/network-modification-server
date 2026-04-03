package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

public interface CompositeModificationRepository extends JpaRepository<CompositeModificationEntity, UUID> {

    @Modifying
    @Query(value = """
        DELETE FROM composite_modification_sub_modifications
        WHERE id = :compositeId
        """, nativeQuery = true)
    void deleteCompositeSubModifications(@Param("compositeId") UUID compositeId);

    @Modifying
    @Query(value = """
        INSERT INTO composite_modification_sub_modifications (id, modification_id, modifications_order)
        VALUES (:compositeId, :modificationId, :position)
        """, nativeQuery = true)
    void insertCompositeSubModification(
            @Param("compositeId") UUID compositeId,
            @Param("modificationId") UUID modificationId,
            @Param("position") int position);
}
