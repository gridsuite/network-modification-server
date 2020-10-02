/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RunWith(SpringRunner.class)
@WebMvcTest(NetworkModificationController.class)
@ContextConfiguration(classes = {NetworkModificationApplication.class})
public class NetworkModificationTest {

    @Autowired
    private MockMvc mvc;

    @MockBean
    private NetworkStoreService networkStoreService;

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void test() throws Exception {
        UUID testNetworkId = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
        UUID notFoundNetworkId = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");

        given(networkStoreService.getNetwork(testNetworkId)).willReturn(createNetwork());
        given(networkStoreService.getNetwork(notFoundNetworkId)).willThrow(new PowsyblException());

        // network not existing
        mvc.perform(put("/v1/networks/{networkUuid}/switches/{switchId}/", notFoundNetworkId, "v1b1").param("open", "true"))
                .andExpect(status().isNotFound());

        // switch not existing
        mvc.perform(put("/v1/networks/{networkUuid}/switches/{switchId}", testNetworkId, "notFound").param("open", "true"))
                .andExpect(status().isNotFound());

        // switch opening
        mvc.perform(put("/v1/networks/{networkUuid}/switches/{switchId}", testNetworkId, "v1b1").param("open", "true"))
                .andExpect(status().isOk());

        // switch closing
        mvc.perform(put("/v1/networks/{networkUuid}/switches/{switchId}", testNetworkId, "v2b1").param("open", "false"))
                .andExpect(status().isOk());

        // lockout a non-existing line
        MvcResult mvcResult = mvc.perform(put("/v1/networks/{networkUuid}/lines/{lineId}/switches", testNetworkId, "nonExistingLineId").param("lockout", "true"))
                .andExpect(status().isNotFound())
                .andReturn();
        assertEquals("Line nonExistingLineId not found", mvcResult.getResponse().getErrorMessage());

        // lockout a line
        mvc.perform(put("/v1/networks/{networkUuid}/lines/{lineId}/switches", testNetworkId, "lineId").param("lockout", "true"))
                .andExpect(status().isOk())
                .andReturn();

        // lockout a line
        mvc.perform(put("/v1/networks/{networkUuid}/lines/{lineId}/switches", testNetworkId, "lineId").param("lockout", "false"))
                .andExpect(status().isOk())
                .andReturn();
    }

    public static Network createNetwork() {
        Network network = Network.create("test", "test");
        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1, "1.1", "1.1", 0);
        createSwitch(v1, "v1d1", "v1d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "v1b1", "v1b1", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v1, "v1load", "v1load", 2, 0., 0.);

        VoltageLevel v2 = createVoltageLevel(s1, "v2", "v2", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v2, "1A", "1A", 0);
        createBusBarSection(v2, "1B", "1B", 1);
        createSwitch(v2, "v2d1", "v2d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v2, "v2b1", "v2b1", SwitchKind.BREAKER, true, true, false, 2, 3);
        createSwitch(v2, "v2d2", "v2d2", SwitchKind.DISCONNECTOR, true, false, false, 3, 1);
        createSwitch(v2, "v2dload", "v2dload", SwitchKind.DISCONNECTOR, true, false, false, 1, 4);
        createSwitch(v2, "v2bload", "v2bload", SwitchKind.BREAKER, true, false, false, 4, 5);
        createLoad(v2, "v2load", "v2load", 5, 0., 0.);

        VoltageLevel v3 = createVoltageLevel(s1, "v3", "v3", TopologyKind.NODE_BREAKER, 380.0);
        VoltageLevel v4 = createVoltageLevel(s1, "v4", "v4", TopologyKind.NODE_BREAKER, 380.0);
        createLine(network, "lineId", "lineName", v3.getId(), v4.getId(), 0, 1, 1, 1, 1, 1, 1, 1);

        return network;
    }

    private static Substation createSubstation(Network n, String id, String name, Country country) {
        return n.newSubstation()
                .setId(id)
                .setName(name)
                .setCountry(country)
                .add();
    }

    private static VoltageLevel createVoltageLevel(Substation s, String id, String name,
                                                     TopologyKind topology, double vNom) {
        return s.newVoltageLevel()
                .setId(id)
                .setName(name)
                .setTopologyKind(topology)
                .setNominalV(vNom)
                .add();
    }

    private static void createBusBarSection(VoltageLevel vl, String id, String name, int node) {
        BusbarSection bbs = vl.getNodeBreakerView().newBusbarSection()
                .setId(id)
                .setName(name)
                .setNode(node)
                .add();
    }

    private static void createSwitch(VoltageLevel vl, String id, String name, SwitchKind kind, boolean retained, boolean open, boolean fictitious, int node1, int node2) {
        vl.getNodeBreakerView().newSwitch()
                .setId(id)
                .setName(name)
                .setKind(kind)
                .setRetained(retained)
                .setOpen(open)
                .setFictitious(fictitious)
                .setNode1(node1)
                .setNode2(node2)
                .add();
    }

    private static void createLoad(VoltageLevel vl, String id, String name,
                                   int node, double p0, double q0) {
        Load load = vl.newLoad()
                .setId(id)
                .setName(name)
                .setNode(node)
                .setP0(p0)
                .setQ0(q0)
                .add();
    }

    private static void createLine(Network network, String id, String name,
                                   String v1, String v2, int node1, int node2,
                                   double b1, double b2, double x, double r,
                                   double g1, double g2) {
        network.newLine()
                .setId(id)
                .setName(name)
                .setVoltageLevel1(v1)
                .setVoltageLevel2(v2)
                .setNode1(node1)
                .setNode2(node2)
                .setB1(b1)
                .setB2(b2)
                .setX(x)
                .setR(r)
                .setG1(g1)
                .setG2(g2)
                .add();
    }
}
