/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.google.common.collect.Iterables;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.EquipmentType;
import org.gridsuite.modification.server.dto.VoltageLevelInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {"spring.data.elasticsearch.enabled=true"})
public class EquipmentInfosServiceTests {

    private static final UUID NETWORK_UUID = UUID.fromString("38400000-8cf0-11bd-b23e-10b96e4ef00d");

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Test
    public void testAddDeleteEquipmentInfos() {
        EqualsVerifier.simple().forClass(EquipmentInfos.class).verify();
        EqualsVerifier.simple().forClass(VoltageLevelInfos.class).verify();

        EquipmentInfos equipmentInfos = EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant1").id("id1").name("name1").type(EquipmentType.LOAD.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build();
        assertEquals(equipmentInfosService.add(equipmentInfos), equipmentInfos);

        equipmentInfosService.deleteEquipmentInVariant(equipmentInfos.getId(), NETWORK_UUID, "variant1");
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        Set<String> ids = new HashSet<>();
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id1").name("name1").type(EquipmentType.LOAD.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id2").name("name2").type(EquipmentType.GENERATOR.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl2").name("vl2").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id3").name("name3").type(EquipmentType.BREAKER.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl3").name("vl3").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id4").name("name4").type(EquipmentType.HVDC_LINE.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl4").name("vl4").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id5").name("name5").type(EquipmentType.SUBSTATION.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl5").name("vl5").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id6").name("name6").type(EquipmentType.VOLTAGE_LEVEL.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl6").name("vl6").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant2").id("id7").name("name6").type(EquipmentType.CONFIGURED_BUS.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl7").name("vl7").build())).build());
        assertEquals(7, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        ids.forEach(id -> equipmentInfosService.deleteEquipmentInVariant(id, NETWORK_UUID, "variant2"));
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        ids.clear();
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant1").id("idOk").name("name1").type(EquipmentType.LOAD.name()).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant1").id("idTombstoned").name("name2").type(EquipmentType.GENERATOR.name()).tombstoned(true).build());
        assertTrue(equipmentInfosService.existEquipmentInVariant("idOk", NETWORK_UUID, "variant1"));
        assertFalse(equipmentInfosService.existEquipmentInVariant("idTombstoned", NETWORK_UUID, "variant1"));

        ids.forEach(id -> equipmentInfosService.deleteEquipmentInVariant(id, NETWORK_UUID, "variant1"));
        assertEquals(1, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1"));
    }

    private void addEquipmentInfos(Set<String> ids, EquipmentInfos equipmentInfos) {
        ids.add(equipmentInfosService.add(equipmentInfos).getId());
    }

    @Test
    public void testCloneVariant() {
        equipmentInfosService.add(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(EquipmentType.LOAD.name()).variantId("variant1").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        assertEquals(1, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.cloneVariantModifications(NETWORK_UUID, "variant1", "variant2");
        assertEquals(2, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1", "variant2"));
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));
    }

    @Test
    public void testDeleteVariants() {
        equipmentInfosService.add(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(EquipmentType.LOAD.name()).variantId("variant1").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        assertEquals(1, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.add(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id2").name("name2").type(EquipmentType.GENERATOR.name()).variantId("variant2").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl2").name("vl2").build())).build());
        assertEquals(2, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.add(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id3").name("name3").type(EquipmentType.BATTERY.name()).variantId("variant3").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl3").name("vl3").build())).build());
        assertEquals(3, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1", "variant3"));
        Iterable<EquipmentInfos> equipments = equipmentInfosService.findAll(NETWORK_UUID);
        assertEquals(1, Iterables.size(equipments));
        assertTrue(Iterables.get(equipments, 0).getVariantId().equals("variant2"));

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant2"));
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));
    }

    @Test
    public void testEquipmentType() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);

        assertEquals(EquipmentType.SUBSTATION, EquipmentType.getType(network.getSubstation("s1")));
        assertEquals(EquipmentType.VOLTAGE_LEVEL, EquipmentType.getType(network.getVoltageLevel("v1")));
        assertEquals(EquipmentType.BREAKER, EquipmentType.getType(network.getSwitch("v1b1")));
        assertEquals(EquipmentType.LOAD, EquipmentType.getType(network.getLoad("v1load")));
        assertEquals(EquipmentType.HVDC_LINE, EquipmentType.getType(network.getHvdcLine("hvdcLine")));
        assertEquals(EquipmentType.LINE, EquipmentType.getType(network.getLine("line1")));
        assertEquals(EquipmentType.TWO_WINDINGS_TRANSFORMER, EquipmentType.getType(network.getTwoWindingsTransformer("trf1")));
        assertEquals(EquipmentType.THREE_WINDINGS_TRANSFORMER, EquipmentType.getType(network.getThreeWindingsTransformer("trf6")));

        network = NetworkCreation.createBusBreaker(NETWORK_UUID);
        assertEquals(EquipmentType.SUBSTATION, EquipmentType.getType(network.getSubstation("s1")));
        assertEquals(EquipmentType.VOLTAGE_LEVEL, EquipmentType.getType(network.getVoltageLevel("v1")));
        assertEquals(EquipmentType.CONFIGURED_BUS, EquipmentType.getType(network.getBusBreakerView().getBus("bus1")));
        assertEquals(EquipmentType.GENERATOR, EquipmentType.getType(network.getGenerator("idGenerator1")));
    }

    @Test
    public void testVoltageLevels() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build(), VoltageLevelInfos.builder().id("v4").name("v4").build()), EquipmentInfos.getVoltageLevels(network.getSubstation("s1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getVoltageLevel("v1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getSwitch("v1b1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getLoad("v1load")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build()), EquipmentInfos.getVoltageLevels(network.getHvdcLine("hvdcLine")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v3").name("v3").build(), VoltageLevelInfos.builder().id("v4").name("v4").build()), EquipmentInfos.getVoltageLevels(network.getLine("line1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build()), EquipmentInfos.getVoltageLevels(network.getTwoWindingsTransformer("trf1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build(), VoltageLevelInfos.builder().id("v4").name("v4").build()), EquipmentInfos.getVoltageLevels(network.getThreeWindingsTransformer("trf6")));

        network = NetworkCreation.createBusBreaker(NETWORK_UUID);
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v12").name("v12").build(), VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getSubstation("s1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getVoltageLevel("v1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getBusBreakerView().getBus("bus1")));
        assertEquals(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()), EquipmentInfos.getVoltageLevels(network.getGenerator("idGenerator1")));
    }

    @Test
    public void testBadType() {
        Identifiable<Network> network = new NetworkFactoryImpl().createNetwork("test", "test");
        String errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentType.getType(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));

        errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getVoltageLevels(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));
    }
}
