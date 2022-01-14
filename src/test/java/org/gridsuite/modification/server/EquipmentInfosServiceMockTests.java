/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.google.common.collect.Iterables;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.VoltageLevelInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertEquals;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {"spring.data.elasticsearch.enabled=false"})
public class EquipmentInfosServiceMockTests {

    private static final UUID NETWORK_UUID = UUID.fromString("38400000-8cf0-11bd-b23e-10b96e4ef00d");

    private static final String VARIANT_ID = "variant1";

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Test
    public void testAddDeleteEquipmentInfos() {
        equipmentInfosService.add(EquipmentInfos.builder().networkUuid(NETWORK_UUID).id("id1").variantId(VARIANT_ID).name("name1").type(IdentifiableType.LOAD.name()).voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build())).build());
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));

        equipmentInfosService.delete("foo", NETWORK_UUID, VARIANT_ID);
        assertEquals(0, Iterables.size(equipmentInfosService.findAll(NETWORK_UUID)));
    }
}
