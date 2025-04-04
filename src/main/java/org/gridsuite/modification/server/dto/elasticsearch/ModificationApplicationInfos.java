/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.elasticsearch;

import lombok.*;
import org.gridsuite.modification.server.modifications.ImpactedEquipmentsInfos;
import org.springframework.data.annotation.AccessType;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Setting;

import java.util.List;
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
@Document(indexName = "#{@environment.getProperty('powsybl-ws.elasticsearch.index.prefix')}modifications")
@Setting(settingPath = "elasticsearch_settings.json")
@TypeAlias(value = "BasicModificationInfos")
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

    private List<String> createdEquipmentIds;
    private List<String> modifiedEquipmentIds;
    private List<String> deletedEquipmentIds;

    @Transient
    @Builder.Default
    ImpactedEquipmentsInfos impactedEquipmentsInfos = new ImpactedEquipmentsInfos();

    public ModificationApplicationInfos flushImpactedEquipments() {
        createdEquipmentIds = impactedEquipmentsInfos.getCreatedEquipments().stream().map(BasicEquipmentInfos::getId).collect(Collectors.toList());
        modifiedEquipmentIds = impactedEquipmentsInfos.getModifiedEquipments().stream().map(BasicEquipmentInfos::getId).collect(Collectors.toList());
        deletedEquipmentIds = impactedEquipmentsInfos.getTombstonedEquipments().stream().map(BasicEquipmentInfos::getId).collect(Collectors.toList());
        impactedEquipmentsInfos = null;
        return this;
    }
}
