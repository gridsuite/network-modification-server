/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.*;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.dto.SubstationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.TombstonedEquipmentInfosRepository;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Tag("IntegrationTest")
class EquipmentInfosServiceTests {

    private static final UUID NETWORK_UUID = UUID.randomUUID();

    private static final String VARIANT_NAME_1 = "variant1";
    private static final String VARIANT_NAME_2 = "variant2";
    private static final String VARIANT_NAME_3 = "variant3";

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Autowired
    private EquipmentInfosRepository equipmentInfosRepository;

    @Autowired
    private TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    private boolean existEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return equipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantId).stream().anyMatch(t -> t.getId().equals(equipmentId));
    }

    @Test
    void testAddDeleteEquipmentInfos() {
        EquipmentInfos equipmentInfos = EquipmentInfos.builder()
                .networkUuid(NETWORK_UUID)
                .id("id1")
                .variantId(VARIANT_NAME_1)
                .name("name1")
                .type(IdentifiableType.LOAD.name())
                .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build()))
                .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
                .build();
        equipmentInfosService.addAllEquipmentInfos(List.of(equipmentInfos));
        List<EquipmentInfos> infosDB = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1);
        assertEquals(1, infosDB.size());
        assertEquals(equipmentInfos, infosDB.get(0));
        assertEquals(equipmentInfos.getNetworkUuid() + "_" + equipmentInfos.getVariantId() + "_" + equipmentInfos.getId(), infosDB.get(0).getUniqueId());

        // Change name but uniqueIds are same
        equipmentInfos = EquipmentInfos.builder()
            .networkUuid(NETWORK_UUID)
            .id("id1")
            .variantId(VARIANT_NAME_1)
            .name("newName")
            .type(IdentifiableType.LOAD.name())
            .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build()))
            .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
            .build();
        equipmentInfosService.addAllEquipmentInfos(List.of(equipmentInfos));
        infosDB = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1);
        assertEquals(1, infosDB.size());
        assertEquals(equipmentInfos, infosDB.get(0));
        assertEquals(equipmentInfos.getNetworkUuid() + "_" + equipmentInfos.getVariantId() + "_" + equipmentInfos.getId(), infosDB.get(0).getUniqueId());

        equipmentInfosRepository.deleteByIdInAndNetworkUuidAndVariantId(List.of(equipmentInfos.getId()), NETWORK_UUID, VARIANT_NAME_1);
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());

        List<EquipmentInfos> equipmentInfosList = List.of(
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id1")
                    .variantId(VARIANT_NAME_2)
                    .name("name1")
                    .type(IdentifiableType.LOAD.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
                    .build(),
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id2")
                    .variantId(VARIANT_NAME_2)
                    .name("name2")
                    .type(IdentifiableType.GENERATOR.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl2").name("vl2").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s2").name("s2").build()))
                    .build(),
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id3")
                    .variantId(VARIANT_NAME_2)
                    .name("name3")
                    .type(IdentifiableType.SWITCH.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl3").name("vl3").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s3").name("s3").build()))
                    .build(),
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id4")
                    .variantId(VARIANT_NAME_2)
                    .name("name4")
                    .type(IdentifiableType.HVDC_LINE.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl4").name("vl4").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s4").name("s4").build()))
                    .build(),
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id5")
                    .variantId(VARIANT_NAME_2)
                    .name("name5")
                    .type(IdentifiableType.SUBSTATION.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl5").name("vl5").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s5").name("s5").build()))
                    .build(),
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id6")
                    .variantId(VARIANT_NAME_2)
                    .name("name6")
                    .type(IdentifiableType.VOLTAGE_LEVEL.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl6").name("vl6").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s6").name("s6").build()))
                    .build(),
            EquipmentInfos.builder()
                    .networkUuid(NETWORK_UUID)
                    .id("id7")
                    .variantId(VARIANT_NAME_2)
                    .name("name6")
                    .type(IdentifiableType.BUS.name())
                    .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl7").name("vl7").build()))
                    .substations(Set.of(SubstationInfos.builder().id("s7").name("s7").build()))
                    .build()
        );
        equipmentInfosService.addAllEquipmentInfos(equipmentInfosList);
        assertEquals(7, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_2).size());

        equipmentInfosService.deleteEquipmentInfosList(equipmentInfosList.stream().map(EquipmentInfos::getId).collect(Collectors.toList()), NETWORK_UUID, VARIANT_NAME_2);
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_2).size());

        equipmentInfosService.addAllEquipmentInfos(List.of(EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId(VARIANT_NAME_1).id("idOk").name("name1").type(IdentifiableType.LOAD.name()).build()));
        assertTrue(existEquipmentInfos("idOk", NETWORK_UUID, VARIANT_NAME_1));

        TombstonedEquipmentInfos tombstonedEquipmentInfos = TombstonedEquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId(VARIANT_NAME_1).id("idTombstoned").build();
        equipmentInfosService.addAllTombstonedEquipmentInfos(List.of(tombstonedEquipmentInfos));
        assertFalse(existEquipmentInfos("idTombstoned", NETWORK_UUID, VARIANT_NAME_1));
        assertNotNull(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).get(0));
        assertEquals(1, tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());
        assertEquals(tombstonedEquipmentInfos, tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).get(0));

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of(VARIANT_NAME_1));
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());
        assertEquals(0, tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());
    }

    @Test
    void testCloneVariant() {
        equipmentInfosService.addAllEquipmentInfos(List.of(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(IdentifiableType.LOAD.name()).variantId(VARIANT_NAME_1).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build())).build()));
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());

        equipmentInfosService.cloneVariantModifications(NETWORK_UUID, VARIANT_NAME_1, VARIANT_NAME_2);
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_2).size());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of(VARIANT_NAME_1, VARIANT_NAME_2));
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());
    }

    @Test
    void testDeleteVariants() {
        equipmentInfosService.addAllEquipmentInfos(List.of(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(IdentifiableType.LOAD.name()).variantId(VARIANT_NAME_1).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build())).build()));
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());

        equipmentInfosService.addAllEquipmentInfos(List.of(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id2").name("name2").type(IdentifiableType.GENERATOR.name()).variantId(VARIANT_NAME_2).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl2").name("vl2").build())).substations(Set.of(SubstationInfos.builder().id("s2").name("s2").build())).build()));
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_2).size());

        equipmentInfosService.addAllEquipmentInfos(List.of(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id3").name("name3").type(IdentifiableType.BATTERY.name()).variantId(VARIANT_NAME_3).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl3").name("vl3").build())).substations(Set.of(SubstationInfos.builder().id("s3").name("s3").build())).build()));
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_3).size());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of(VARIANT_NAME_1, VARIANT_NAME_3));
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_1).size());
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_3).size());
        List<EquipmentInfos> equipments = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_2);
        assertEquals(1, equipments.size());
        assertEquals(VARIANT_NAME_2, equipments.get(0).getVariantId());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of(VARIANT_NAME_2));
        assertEquals(0, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, VARIANT_NAME_2).size());
    }

    @Test
    void testVoltageLevels() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build(), VoltageLevelInfos.builder().id("v4").name("v4").build()), EquipmentInfos.getVoltageLevelsInfos(network.getSubstation("s1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getVoltageLevel("v1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getSwitch("v1b1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getLoad("v1load")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build()), EquipmentInfos.getVoltageLevelsInfos(network.getHvdcLine("hvdcLine")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v3").name("v3").build(), VoltageLevelInfos.builder().id("v4").name("v4").build()), EquipmentInfos.getVoltageLevelsInfos(network.getLine("line1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build()), EquipmentInfos.getVoltageLevelsInfos(network.getTwoWindingsTransformer("trf1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build(), VoltageLevelInfos.builder().id("v4").name("v4").build()), EquipmentInfos.getVoltageLevelsInfos(network.getThreeWindingsTransformer("trf6")));

        network = NetworkCreation.createBusBreaker(NETWORK_UUID);
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v12").name("v12").build(), VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getSubstation("s1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getVoltageLevel("v1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getBusBreakerView().getBus("bus1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevelsInfos(network.getGenerator("idGenerator1")));
    }

    @Test
    void testSubstations() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getSubstation("s1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getVoltageLevel("v1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getSwitch("v1b1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getLoad("v1load")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getHvdcLine("hvdcLine")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build(), SubstationInfos.builder().id("s2").name("s2").build()), EquipmentInfos.getSubstationsInfos(network.getLine("line1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getTwoWindingsTransformer("trf1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getThreeWindingsTransformer("trf6")));

        network = NetworkCreation.createBusBreaker(NETWORK_UUID);
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getSubstation("s1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getVoltageLevel("v1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getBusBreakerView().getBus("bus1")));
        assertEquals(Set.of(SubstationInfos.builder().id("s1").name("s1").build()), EquipmentInfos.getSubstationsInfos(network.getGenerator("idGenerator1")));
    }

    @Test
    void testBadType() {
        Identifiable<Network> network = new NetworkFactoryImpl().createNetwork("test", "test");

        String errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getVoltageLevelsInfos(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));

        errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getSubstationsInfos(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));

        errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getSubstationsInfos(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));
    }

    @Test
    void testUnsupportedHybridHvdc() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);
        HvdcLine hvdcLine = network.getHvdcLine("hvdcLine");
        String errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getEquipmentTypeName(hvdcLine)).getMessage();
        assertEquals(NetworkModificationException.createHybridHvdcUnsupported(hvdcLine.getId()).getMessage(), errorMessage);
    }

    @AfterEach
    public void tearDown() {
        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of(VARIANT_NAME_1, VARIANT_NAME_2, VARIANT_NAME_3));
    }
}
