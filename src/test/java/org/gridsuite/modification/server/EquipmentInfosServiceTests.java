/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.dto.VoltageLevelInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.After;
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
        EqualsVerifier.simple().forClass(TombstonedEquipmentInfos.class).verify();
        EqualsVerifier.simple().forClass(VoltageLevelInfos.class).verify();

        EquipmentInfos equipmentInfos = EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").variantId("variant1").name("name1").type(IdentifiableType.LOAD.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build();
        assertEquals(equipmentInfosService.addEquipmentInfos(equipmentInfos), equipmentInfos);

        equipmentInfosService.deleteEquipmentInfos(equipmentInfos.getId(), NETWORK_UUID, "variant1");
        assertEquals(0, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        Set<String> ids = new HashSet<>();
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").variantId("variant2").name("name1").type(IdentifiableType.LOAD.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id2").variantId("variant2").name("name2").type(IdentifiableType.GENERATOR.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl2").name("vl2").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id3").variantId("variant2").name("name3").type(IdentifiableType.SWITCH.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl3").name("vl3").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id4").variantId("variant2").name("name4").type(IdentifiableType.HVDC_LINE.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl4").name("vl4").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id5").variantId("variant2").name("name5").type(IdentifiableType.SUBSTATION.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl5").name("vl5").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id6").variantId("variant2").name("name6").type(IdentifiableType.VOLTAGE_LEVEL.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl6").name("vl6").build())).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id7").variantId("variant2").name("name6").type(IdentifiableType.BUS.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl7").name("vl7").build())).build());
        assertEquals(7, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        ids.forEach(id -> equipmentInfosService.deleteEquipmentInfos(id, NETWORK_UUID, "variant2"));
        assertEquals(0, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        Set<String> idsTombstoned = new HashSet<>();
        ids.clear();
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant1").id("idOk").name("name1").type(IdentifiableType.LOAD.name()).build());
        addTombstonedEquipmentInfos(idsTombstoned, TombstonedEquipmentInfos.builder().networkUuid(NETWORK_UUID).variantId("variant1").id("idTombstoned").build());
        assertTrue(equipmentInfosService.existEquipmentInfos("idOk", NETWORK_UUID, "variant1"));
        assertFalse(equipmentInfosService.existEquipmentInfos("idTombstoned", NETWORK_UUID, "variant1"));
        assertEquals(1, equipmentInfosService.findAllTombstonedEquipmentInfos(NETWORK_UUID).size());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1"));
        assertEquals(0, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());
        assertEquals(0, equipmentInfosService.findAllTombstonedEquipmentInfos(NETWORK_UUID).size());
    }

    private void addEquipmentInfos(Set<String> ids, EquipmentInfos equipmentInfos) {
        ids.add(equipmentInfosService.addEquipmentInfos(equipmentInfos).getId());
    }

    private void addTombstonedEquipmentInfos(Set<String> ids, TombstonedEquipmentInfos tombstonedEquipmentInfos) {
        ids.add(equipmentInfosService.addTombstonedEquipmentInfos(tombstonedEquipmentInfos).getId());
    }

    @Test
    public void testCloneVariant() {
        equipmentInfosService.addEquipmentInfos(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(IdentifiableType.LOAD.name()).variantId("variant1").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        assertEquals(1, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        equipmentInfosService.cloneVariantModifications(NETWORK_UUID, "variant1", "variant2");
        assertEquals(2, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1", "variant2"));
        assertEquals(0, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());
    }

    @Test
    public void testDeleteVariants() {
        equipmentInfosService.addEquipmentInfos(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(IdentifiableType.LOAD.name()).variantId("variant1").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        assertEquals(1, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        equipmentInfosService.addEquipmentInfos(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id2").name("name2").type(IdentifiableType.GENERATOR.name()).variantId("variant2").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl2").name("vl2").build())).build());
        assertEquals(2, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        equipmentInfosService.addEquipmentInfos(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id3").name("name3").type(IdentifiableType.BATTERY.name()).variantId("variant3").voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl3").name("vl3").build())).build());
        assertEquals(3, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1", "variant3"));
        List<EquipmentInfos> equipments = equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID);
        assertEquals(1, equipments.size());
        assertEquals("variant2", equipments.get(0).getVariantId());

        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant2"));
        assertEquals(0, equipmentInfosService.findAllEquipmentInfos(NETWORK_UUID).size());
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

        String errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getVoltageLevels(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));
    }

    @After
    public void tearDown() {
        equipmentInfosService.deleteVariants(NETWORK_UUID, List.of("variant1", "variant2", "variant3"));
    }
}
