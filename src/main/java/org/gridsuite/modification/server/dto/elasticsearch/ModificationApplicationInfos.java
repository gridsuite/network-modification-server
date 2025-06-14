/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.elasticsearch;

import lombok.*;
import org.gridsuite.modification.server.elasticsearch.ESConfig;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.ImpactedEquipmentsInfos;
import org.gridsuite.modification.server.modifications.IndexedImpactedEquipmentInfos;
import org.springframework.data.annotation.AccessType;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.*;

import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Builder
@AllArgsConstructor
@Getter
@Setter
@ToString
@EqualsAndHashCode
@Document(indexName = ESConfig.MODIFICATIONS_INDEX_NAME)
@Setting(settingPath = "elasticsearch_settings.json")
@TypeAlias(value = "ModificationApplicationInfos")
public class ModificationApplicationInfos {
    @Id
    @AccessType(AccessType.Type.PROPERTY)
    @SuppressWarnings("unused")
    public String getUniqueId() {
        return modificationUuid + "_" + networkUuid;
    }

    @SuppressWarnings("unused")
    public void setUniqueId(String uniqueId) {
        // No setter because it is a composite value
    }

    private UUID modificationUuid;
    private UUID networkUuid;
    private UUID groupUuid;

    @MultiField(
            mainField = @Field(type = FieldType.Text),
            otherFields = {
                @InnerField(suffix = "fullascii", type = FieldType.Keyword, normalizer = "fullascii"),
                @InnerField(suffix = "raw", type = FieldType.Keyword)
            }
    )
    private Set<String> createdEquipmentIds;

    @MultiField(
            mainField = @Field(type = FieldType.Text),
            otherFields = {
                @InnerField(suffix = "fullascii", type = FieldType.Keyword, normalizer = "fullascii"),
                @InnerField(suffix = "raw", type = FieldType.Keyword)
            }
    )
    private Set<String> modifiedEquipmentIds;

    @MultiField(
            mainField = @Field(type = FieldType.Text),
            otherFields = {
                @InnerField(suffix = "fullascii", type = FieldType.Keyword, normalizer = "fullascii"),
                @InnerField(suffix = "raw", type = FieldType.Keyword)
            }
    )
    private Set<String> deletedEquipmentIds;

    @Transient
    @Builder.Default
    ImpactedEquipmentsInfos impactedEquipmentsInfos = new ImpactedEquipmentsInfos();

    @Transient
    ModificationEntity modification;

    public ModificationApplicationInfos flushImpactedEquipments() {
        createdEquipmentIds = impactedEquipmentsInfos.getCreatedEquipments().stream()
            .filter(IndexedImpactedEquipmentInfos::shouldIndexInModification)
            .map(IndexedImpactedEquipmentInfos::impactedEquipmentInfos)
            .map(BasicEquipmentInfos::getId)
            .collect(Collectors.toSet());
        modifiedEquipmentIds = impactedEquipmentsInfos.getModifiedEquipments().stream()
            .filter(IndexedImpactedEquipmentInfos::shouldIndexInModification)
            .map(IndexedImpactedEquipmentInfos::impactedEquipmentInfos)
            .map(BasicEquipmentInfos::getId)
            .collect(Collectors.toSet());
        deletedEquipmentIds = impactedEquipmentsInfos.getTombstonedEquipments().stream()
            .filter(IndexedImpactedEquipmentInfos::shouldIndexInModification)
            .map(IndexedImpactedEquipmentInfos::impactedEquipmentInfos)
            .map(BasicEquipmentInfos::getId)
            .collect(Collectors.toSet());
        impactedEquipmentsInfos = null;
        return this;
    }

    public boolean hasAnyImpactedEquipment() {
        return impactedEquipmentsInfos.hasAnyImpactedEquipmentToIndexInModification();
    }
}
