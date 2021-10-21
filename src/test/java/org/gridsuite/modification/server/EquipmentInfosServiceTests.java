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
        assertEquals(6, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        ids.forEach(id -> equipmentInfosService.delete(id, NETWORK_UUID));
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));
    }

    private void addEquipmentInfos(Set<String> ids, EquipmentInfos equipmentInfos) {
        ids.add(equipmentInfosService.add(equipmentInfos).getId());
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
