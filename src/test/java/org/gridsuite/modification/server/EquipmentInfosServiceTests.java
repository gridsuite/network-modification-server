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
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.HashSet;
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

        EquipmentInfos equipmentInfos = EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(EquipmentType.LOAD.name()).voltageLevelsIds(Set.of("vl1")).build();
        assertEquals(equipmentInfosService.add(equipmentInfos), equipmentInfos);

        equipmentInfosService.delete(equipmentInfos.getId(), NETWORK_UUID);
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        Set<String> ids = new HashSet<>();
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").name("name1").type(EquipmentType.LOAD.name()).voltageLevelsIds(Set.of("vl1")).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id2").name("name2").type(EquipmentType.GENERATOR.name()).voltageLevelsIds(Set.of("vl2")).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id3").name("name3").type(EquipmentType.BREAKER.name()).voltageLevelsIds(Set.of("vl3")).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id4").name("name4").type(EquipmentType.HVDC_LINE.name()).voltageLevelsIds(Set.of("vl4")).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id5").name("name5").type(EquipmentType.SUBSTATION.name()).voltageLevelsIds(Set.of("vl5")).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id6").name("name6").type(EquipmentType.VOLTAGE_LEVEL.name()).voltageLevelsIds(Set.of("vl6")).build());
        addEquipmentInfos(ids, EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id7").name("name6").type(EquipmentType.CONFIGURED_BUS.name()).voltageLevelsIds(Set.of("vl7")).build());
        assertEquals(7, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        ids.forEach(id -> equipmentInfosService.delete(id, NETWORK_UUID));
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));
    }

    private void addEquipmentInfos(Set<String> ids, EquipmentInfos equipmentInfos) {
        ids.add(equipmentInfosService.add(equipmentInfos).getId());
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
    public void testVoltageLevelsIds() {
        Network network = NetworkCreation.create(NETWORK_UUID, true);
        assertEquals(Set.of("v1", "v2", "v4"), EquipmentInfos.getVoltageLevelsIds(network.getSubstation("s1")));
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getVoltageLevel("v1")));
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getSwitch("v1b1")));
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getLoad("v1load")));
        assertEquals(Set.of("v1", "v2"), EquipmentInfos.getVoltageLevelsIds(network.getHvdcLine("hvdcLine")));
        assertEquals(Set.of("v3", "v4"), EquipmentInfos.getVoltageLevelsIds(network.getLine("line1")));
        assertEquals(Set.of("v1", "v2"), EquipmentInfos.getVoltageLevelsIds(network.getTwoWindingsTransformer("trf1")));
        assertEquals(Set.of("v4", "v2", "v1"), EquipmentInfos.getVoltageLevelsIds(network.getThreeWindingsTransformer("trf6")));

        network = NetworkCreation.createBusBreaker(NETWORK_UUID);
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getSubstation("s1")));
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getVoltageLevel("v1")));
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getBusBreakerView().getBus("bus1")));
        assertEquals(Set.of("v1"), EquipmentInfos.getVoltageLevelsIds(network.getGenerator("idGenerator1")));
    }

    @Test
    public void testBadType() {
        Identifiable<Network> network = new NetworkFactoryImpl().createNetwork("test", "test");
        String errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentType.getType(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));

        errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentInfos.getVoltageLevelsIds(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));
    }
}
