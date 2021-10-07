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
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.EquipmentType;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.utils.MatcherEquipmentInfos;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
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
        EquipmentInfos equipmentInfos = EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id1").equipmentName("name1").equipmentType(EquipmentType.LOAD.name()).build();
        assertThat(equipmentInfosService.add(equipmentInfos), new MatcherEquipmentInfos<>(equipmentInfos));

        equipmentInfosService.delete(equipmentInfos.getEquipmentId(), NETWORK_UUID);
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        Set<String> equipmentIds = new HashSet<>();
        addEquipmentInfos(equipmentIds, EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id1").equipmentName("name1").equipmentType(EquipmentType.LOAD.name()).build());
        addEquipmentInfos(equipmentIds, EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id2").equipmentName("name2").equipmentType(EquipmentType.GENERATOR.name()).build());
        addEquipmentInfos(equipmentIds, EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id3").equipmentName("name3").equipmentType(EquipmentType.BREAKER.name()).build());
        addEquipmentInfos(equipmentIds, EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id4").equipmentName("name4").equipmentType(EquipmentType.HVDC.name()).build());
        addEquipmentInfos(equipmentIds, EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id5").equipmentName("name5").equipmentType(EquipmentType.SUBSTATION.name()).build());
        addEquipmentInfos(equipmentIds, EquipmentInfos.builder().networkUuid(NETWORK_UUID).equipmentId("id6").equipmentName("name6").equipmentType(EquipmentType.VOLTAGE_LEVEL.name()).build());
        assertEquals(6, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentIds.forEach(id -> equipmentInfosService.delete(id, NETWORK_UUID));
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));
    }

    private void addEquipmentInfos(Set<String> ids, EquipmentInfos equipmentInfos) {
        ids.add(equipmentInfosService.add(equipmentInfos).getEquipmentId());
    }

    @Test
    public void testBadEquipmentType() {
        Identifiable<Network> network = new NetworkFactoryImpl().createNetwork("test", "test");
        String errorMessage = assertThrows(NetworkModificationException.class, () -> EquipmentType.getType(network)).getMessage();
        assertTrue(errorMessage.contains(String.format("The equipment type : %s is unknown", NetworkImpl.class.getSimpleName())));
    }
}
