/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.*;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.utils.JsonSetConverter;

import java.util.Set;
import java.util.UUID;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@Builder
@Embeddable
@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Entity
@Table(
    name = "modification_application",
    indexes = {
        @Index(name = "idx_modification_application_networkUuid", columnList = "networkUuid")
    }
)
public class ModificationApplicationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "uuid")
    private UUID uuid;

    @Column(name = "network_uuid")
    UUID networkUuid;

    @Column(name = "created_equipment_ids", columnDefinition = "CLOB")
    @Convert(converter = JsonSetConverter.class)
    private Set<String> createdEquipmentIds;

    @Column(name = "modified_equipment_ids", columnDefinition = "CLOB")
    @Convert(converter = JsonSetConverter.class)
    private Set<String> modifiedEquipmentIds;

    @Column(name = "deleted_equipment_ids", columnDefinition = "CLOB")
    @Convert(converter = JsonSetConverter.class)
    private Set<String> deletedEquipmentIds;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "modification_uuid", foreignKey = @ForeignKey(name = "modification_uuid_fk_constraint"))
    ModificationEntity modification;

    public ModificationApplicationInfos toModificationApplicationInfos() {
        return ModificationApplicationInfos.builder()
            .modificationUuid(this.getModification().getId())
            .deletedEquipmentIds(this.getDeletedEquipmentIds())
            .createdEquipmentIds(this.getCreatedEquipmentIds())
            .modifiedEquipmentIds(this.getModifiedEquipmentIds())
            .networkUuid(this.getNetworkUuid())
            .groupUuid(this.getModification().getGroup().getId())
            .build();
    }
}
