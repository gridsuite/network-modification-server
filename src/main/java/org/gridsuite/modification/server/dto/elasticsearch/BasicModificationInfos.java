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
public class BasicModificationInfos {
    @Id
    @AccessType(AccessType.Type.PROPERTY)
    @SuppressWarnings("unused")
    public String getUniqueId() {
        return modificationUuid + "_" + networkUuid;
    }

    @SuppressWarnings("unused")
    public void setUniqueId(String uniqueId) {
        // No setter because it a composite value
    }

    @AccessType(AccessType.Type.PROPERTY)
    public List<String> getImpactedEquipmentUuids() {
        return impactedEquipmentsInfos.getAllEquipmentsIds();
    }

    @SuppressWarnings("unused")
    public void setImpactedEquipmentUuids(List<String> uuids) {
        // No setter because it a calculated value
    }

    private UUID modificationUuid;
    private UUID networkUuid;
    private UUID groupUuid;

    @Transient
    ImpactedEquipmentsInfos impactedEquipmentsInfos;
}
