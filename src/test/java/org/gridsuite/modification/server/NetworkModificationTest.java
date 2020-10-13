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
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import java.util.UUID;

import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
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

        testEquipmentModification(testNetworkId, ModifiableEquipmentType.GENERATOR, "idGenerator", "targetP", "15", Boolean.TRUE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.GENERATOR, "idGenerator", "targetP", "65000", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.GENERATOR, "idGenerator", "targetQ", "15.0", Boolean.TRUE);

        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wP", "phaseTapChanger2Position", "9", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wP", "phaseTapChanger2Position", "2", Boolean.TRUE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wP", "phaseTapChanger1Position", "9", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wP", "phaseTapChanger3Position", "9", Boolean.FALSE);

        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wR", "ratioTapChanger2Position", "9", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wR", "ratioTapChanger1Position", "9", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wR", "ratioTapChanger3Position", "9", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo3wR", "ratioTapChanger3Position", "3", Boolean.TRUE);

        testEquipmentModification(testNetworkId, ModifiableEquipmentType.TWO_WINDINGS_TRANSFORMER, "trfo2wP", "phaseTapChangerPosition", "9", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.TWO_WINDINGS_TRANSFORMER, "trfo2wP", "phaseTapChangerPosition", "4", Boolean.TRUE);

        testEquipmentModification(testNetworkId, ModifiableEquipmentType.TWO_WINDINGS_TRANSFORMER, "trfo2wR", "ratioTapChangerPosition", "99", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.TWO_WINDINGS_TRANSFORMER, "trfo2wR", "ratioTapChangerPosition", "4", Boolean.TRUE);

        testEquipmentModification(testNetworkId, ModifiableEquipmentType.TWO_WINDINGS_TRANSFORMER, "trfo2wR", "__no", "4", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.THREE_WINDINGS_TRANSFORMER, "trfo2wR", "__exising", "4", Boolean.FALSE);
        testEquipmentModification(testNetworkId, ModifiableEquipmentType.GENERATOR, "trfo2wR", "__command", "4", Boolean.FALSE);

        mvc.perform(post("/v1/networks/{networkUuid}/GENERATOR/{generatorId}", testNetworkId, "generatorId")
            .contentType(MediaType.APPLICATION_JSON)
            .content("{}")
            .characterEncoding("utf-8")
            .accept(MediaType.APPLICATION_JSON)
        ).andExpect(status().isBadRequest());
    }

    private void testEquipmentModification(UUID testNetworkId, ModifiableEquipmentType equipmentType, String equipmentId, String cmd, String target, Boolean expectedResult) throws Exception {
        mvc.perform(post("/v1/networks/{networkUuid}/{typeEq}/{generatorId}", testNetworkId, equipmentType, equipmentId)
            .contentType(MediaType.APPLICATION_JSON)
            .content("{ \"" + cmd + "\": \"" + target + "\" }")
            .characterEncoding("utf-8")
            .accept(MediaType.APPLICATION_JSON)
        ).andExpect(status().isOk())
            .andExpect(jsonPath("$." + cmd).value(expectedResult));
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

        int node1 = 3;
        int node2 = 6;
        int node3 = 0;
        VoltageLevel v3 = createVoltageLevel(s1, "v3", "v3", TopologyKind.NODE_BREAKER, 125.0);
        createBusBarSection(v3, "b3", "b3", node3++);
        addPhaseTapChanger(createThreeWindingTransformer(s1, "trfo3wP", "v1", "v2", "v3", node1++, node2++, node3++).getLeg2(), 0, 5);
        addRatioTapChanger(createThreeWindingTransformer(s1, "trfo3wR", "v1", "v2", "v3", node1++, node2++, node3++).getLeg3(), 1, 5);
        addPhaseTapChanger(createTwoWindingTransformer(s1, "trfo2wP", "v1", "v2", node1++, node2++), 0, 3);
        addRatioTapChanger(createTwoWindingTransformer(s1, "trfo2wR", "v1", "v2", node1++, node2++), 3, 4);
        createGenerator(v2, "idGenerator", node2++, 42.1, 1.0);
        return network;
    }

    private static void addPhaseTapChanger(PhaseTapChangerHolder transformer, int lowTap, int currentTap) {
        transformer.newPhaseTapChanger()
            .setLowTapPosition(lowTap)
            .setTapPosition(currentTap)
            .beginStep().setAlpha(1.1).setB(1.2).setG(1.3).setR(1.4).setRho(1.5).setX(1.6).endStep()
            .beginStep().setAlpha(2.2).setB(2.2).setG(2.3).setR(2.4).setRho(2.5).setX(2.6).endStep()
            .beginStep().setAlpha(3.3).setB(3.2).setG(3.3).setR(3.4).setRho(3.5).setX(3.6).endStep()
            .beginStep().setAlpha(4.4).setB(4.2).setG(4.3).setR(4.4).setRho(4.5).setX(4.6).endStep()
            .beginStep().setAlpha(5.5).setB(5.2).setG(5.3).setR(5.5).setRho(5.5).setX(5.6).endStep()
            .beginStep().setAlpha(6.6).setB(6.2).setG(6.3).setR(6.6).setRho(6.5).setX(6.6).endStep()
            .add();
    }

    private static void addRatioTapChanger(RatioTapChangerHolder transformer, int lowTap, int currentTap) {
        transformer.newRatioTapChanger()
            .setLowTapPosition(lowTap)
            .setTapPosition(currentTap)
            .beginStep().setB(1.2).setG(1.3).setR(1.4).setRho(1.5).setX(1.6).endStep()
            .beginStep().setB(2.2).setG(2.3).setR(2.4).setRho(2.5).setX(2.6).endStep()
            .beginStep().setB(3.2).setG(3.3).setR(3.4).setRho(3.5).setX(3.6).endStep()
            .beginStep().setB(4.2).setG(4.3).setR(4.4).setRho(4.5).setX(4.6).endStep()
            .beginStep().setB(5.2).setG(5.3).setR(5.5).setRho(5.5).setX(5.6).endStep()
            .beginStep().setB(6.2).setG(6.3).setR(6.6).setRho(6.5).setX(6.6).endStep()
            .add();
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

    @SuppressWarnings("SameParameterValue")
    private static ThreeWindingsTransformer createThreeWindingTransformer(Substation sub, String id,
                                                                          String vl1, String vl2, String vl3,
                                                                          int node1, int node2, int node3
    ) {
        return sub.newThreeWindingsTransformer()
            .newLeg1().setVoltageLevel(vl1).setR(1.0).setX(2.0).setG(7.0).setB(10.).setRatedU(13.0).setNode(node1).add()
            .newLeg2().setVoltageLevel(vl2).setR(3.0).setX(4.0).setG(8.0).setB(11.).setRatedU(14.0).setNode(node2).add()
            .newLeg3().setVoltageLevel(vl3).setR(5.0).setX(6.0).setG(9.0).setB(12.).setRatedU(15.0).setNode(node3).add()
            .setRatedU0(0.0)
            .setId(id).add();
    }

    @SuppressWarnings("SameParameterValue")
    private static TwoWindingsTransformer createTwoWindingTransformer(Substation sub, String id, String vl1, String vl2,
                                                                    int node1, int node2) {
        return sub.newTwoWindingsTransformer()
            .setId(id)
            .setVoltageLevel1(vl1)
            .setVoltageLevel2(vl2)
            .setNode1(node1)
            .setNode2(node2)
            .setR(1.1)
            .setB(2.2)
            .setG(3.3)
            .setX(4.4)
            .setRatedU1(5.5)
            .setRatedU2(6.6)
            .add();
    }

    @SuppressWarnings("SameParameterValue")
    private static void createGenerator(VoltageLevel vl, String id, int node, double targetP, double targetQ) {
        vl.newGenerator()
            .setId(id)
            .setTargetP(targetP)
            .setTargetQ(targetQ)
            .setNode(node)
            .setMinP(-1.1)
            .setMaxP(1000.0)
            .setVoltageRegulatorOn(false)
            .add();
    }

}
