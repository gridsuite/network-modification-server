/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.GeneratorScalingInfos;
import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.dto.LineModificationInfos;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationMetadataInfos;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.OperationType;
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanEquipmentAttributeModificationEntity;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.ModificationType.LINE_MODIFICATION;
import static org.gridsuite.modification.ModificationType.MODIFICATION_METADATA;

class ModificationEntityMappingTest {

    private final ObjectMapper objectMapper = new ObjectMapper().findAndRegisterModules();

    @Test
    void mapsCreationDtoThroughTheModelHierarchy() throws Exception {
        LoadCreationInfos dto = LoadCreationInfos.builder()
                .equipmentId("load")
                .equipmentName("Load")
                .voltageLevelId("vl")
                .busOrBusbarSectionId("bus")
                .connectionName("feeder")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(3)
                .terminalConnected(true)
                .loadType(LoadType.AUXILIARY)
                .p0(10.)
                .q0(5.)
                .activated(true)
                .build();

        LoadCreationEntity entity = (LoadCreationEntity) ModificationEntity.fromDTO(dto);

        assertThat(entity.getEquipmentId()).isEqualTo("load");
        assertThat(entity.getEquipmentName()).isEqualTo("Load");
        assertThat(entity.getVoltageLevelId()).isEqualTo("vl");
        assertThat(entity.getConnectionPosition()).isEqualTo(3);
        assertThat(entity.getLoadType()).isEqualTo(LoadType.AUXILIARY);
        assertThat(entity.getDate()).isNotNull();
        assertThat(objectMapper.readValue(entity.getMessageValues(), Object.class))
                .isEqualTo(dto.getMapMessageValues());
    }

    @Test
    void mapsBranchDtoThroughTheModelHierarchy() {
        LineCreationInfos dto = LineCreationInfos.builder()
                .equipmentId("line")
                .equipmentName("Line")
                .r(1.)
                .x(2.)
                .g1(3.)
                .b1(4.)
                .g2(5.)
                .b2(6.)
                .voltageLevelId1("vl1")
                .voltageLevelId2("vl2")
                .busOrBusbarSectionId1("bus1")
                .busOrBusbarSectionId2("bus2")
                .operationalLimitsGroups(List.of())
                .build();

        LineCreationEntity entity = (LineCreationEntity) ModificationEntity.fromDTO(dto);

        assertThat(entity.getEquipmentId()).isEqualTo("line");
        assertThat(entity.getR()).isEqualTo(1.);
        assertThat(entity.getVoltageLevelId1()).isEqualTo("vl1");
        assertThat(entity.getG1()).isEqualTo(3.);
    }

    @Test
    void updatesModificationDtoThroughTheModelHierarchy() {
        LineModificationInfos dto = LineModificationInfos.builder()
                .equipmentId("line")
                .r(new AttributeModification<>(1., OperationType.SET))
                .x(new AttributeModification<>(2., OperationType.SET))
                .g1(new AttributeModification<>(3., OperationType.SET))
                .b1(new AttributeModification<>(4., OperationType.SET))
                .g2(new AttributeModification<>(5., OperationType.SET))
                .b2(new AttributeModification<>(6., OperationType.SET))
                .lineSegments(List.of())
                .build();
        LineModificationEntity entity = (LineModificationEntity) ModificationEntity.fromDTO(dto);

        dto.setR(new AttributeModification<>(10., OperationType.SET));
        entity.update(dto);

        assertThat(entity.getR().getValue()).isEqualTo(10.);
        assertThat(entity.getG1().getValue()).isEqualTo(3.);
    }

    @Test
    void mapsScalingDtoThroughItsModelBaseClass() {
        GeneratorScalingInfos dto = GeneratorScalingInfos.builder()
                .variationType(VariationType.TARGET_P)
                .variations(List.of())
                .build();

        GeneratorScalingEntity entity = (GeneratorScalingEntity) ModificationEntity.fromDTO(dto);

        assertThat(entity.getVariationType()).isEqualTo(VariationType.TARGET_P);
        assertThat(entity.getVariations()).isEmpty();
    }

    @Test
    void usesTheAttributeValueFactory() {
        EquipmentAttributeModificationInfos dto = EquipmentAttributeModificationInfos.builder()
                .equipmentId("switch")
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .build();

        ModificationEntity entity = ModificationEntity.fromDTO(dto);

        assertThat(entity).isInstanceOf(BooleanEquipmentAttributeModificationEntity.class);
        assertThat(((BooleanEquipmentAttributeModificationEntity) entity).getAttributeValue()).isTrue();
    }

    @Test
    void createsLightweightMetadataAndDeserializesItToTheConcreteDto() throws Exception {
        UUID id = UUID.randomUUID();
        Instant date = Instant.parse("2026-06-11T10:00:00Z");
        ModificationEntity projection = new ModificationEntity(id, LINE_MODIFICATION.name(), date, false, true,
                LINE_MODIFICATION.name(), "{}", "description");

        ModificationInfos metadata = projection.toModificationInfos();

        assertThat(metadata).isInstanceOf(ModificationMetadataInfos.class);
        assertThat(metadata.getUuid()).isEqualTo(id);
        assertThat(metadata.getDate()).isEqualTo(date);
        assertThat(metadata.getType()).isEqualTo(MODIFICATION_METADATA);
        assertThat(metadata.getDescription()).isEqualTo("description");
        assertThat(metadata.getMessageType()).isEqualTo(LINE_MODIFICATION.name());
        assertThat(metadata.getMessageValues()).isEqualTo("{}");

        String json = objectMapper.writeValueAsString(metadata);
        assertThat(json).doesNotContain("equipmentId", "applySegmentsLimits");
        assertThat(objectMapper.readValue(json, ModificationInfos.class)).isInstanceOf(ModificationMetadataInfos.class);
    }

    @Test
    void deserializesModificationInfosPolymorphically() throws Exception {
        LineCreationInfos dto = LineCreationInfos.builder()
                .equipmentId("line")
                .r(1.)
                .x(2.)
                .build();

        ModificationInfos deserialized = objectMapper.readValue(objectMapper.writeValueAsString(dto), ModificationInfos.class);

        assertThat(deserialized).isInstanceOf(LineCreationInfos.class);
    }

    @Test
    void deserializesNestedModificationInfosPolymorphically() throws Exception {
        CompositeModificationInfos dto = CompositeModificationInfos.builder()
                .name("composite")
                .modificationsInfos(List.of(LineCreationInfos.builder().equipmentId("line").r(1.).x(2.).build()))
                .build();

        ModificationInfos deserialized = objectMapper.readValue(objectMapper.writeValueAsString(dto), ModificationInfos.class);

        assertThat(deserialized).isInstanceOf(CompositeModificationInfos.class);
        assertThat(((CompositeModificationInfos) deserialized).getModificationsInfos().getFirst()).isInstanceOf(LineCreationInfos.class);
    }

    @Test
    void registersEveryModificationTypeForMetadataAndDeserialization() {
        assertThat(ModificationType.values())
                .allSatisfy(type -> {
                    assertThat(EntityRegistry.getDtoClass(type)).isNotNull();
                });
    }
}
