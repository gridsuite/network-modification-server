package org.gridsuite.modification.server.utils;

import java.util.UUID;

import com.powsybl.iidm.network.*;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.powsybl.sld.iidm.extensions.ConnectablePosition;
import com.powsybl.sld.iidm.extensions.ConnectablePositionAdder;

public final class NetworkCreation {
    private NetworkCreation() {
    }

    public static Network create(UUID uuid) {
        Network network = new NetworkFactoryImpl().createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1, "1.1", "1.1", 0);
        createSwitch(v1, "v1d1", "v1d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "v1b1", "v1b1", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v1, "v1load", "v1load", 2, 0., 0.);
        createLccConverterStation(v1, "v1lcc", "v1lcc", 3, 0, 0);

        VoltageLevel v2 = createVoltageLevel(s1, "v2", "v2", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v2, "1A", "1A", 0);
        createBusBarSection(v2, "1B", "1B", 1);
        createSwitch(v2, "v2d1", "v2d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v2, "v2b1", "v2b1", SwitchKind.BREAKER, true, true, false, 2, 3);
        createSwitch(v2, "v2d2", "v2d2", SwitchKind.DISCONNECTOR, true, false, false, 3, 1);
        createSwitch(v2, "v2dload", "v2dload", SwitchKind.DISCONNECTOR, true, false, false, 1, 4);
        createSwitch(v2, "v2bload", "v2bload", SwitchKind.BREAKER, true, false, false, 4, 5);
        createLoad(v2, "v2load", "v2load", 5, 0., 0.);
        createGenerator(v2, "idGenerator", 6, 42.1, 1.0);

        VoltageLevel v4 = createVoltageLevel(s1, "v4", "v4", TopologyKind.NODE_BREAKER, 380.0);

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v3 = createVoltageLevel(s2, "v3", "v3", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v3, "3A", "3A", 0);
        createSwitch(v3, "v3d1", "v3d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "v3b1", "v3b1", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v3, "v3load", "v3load", 2, 0., 0.);

        TwoWindingsTransformer t2 = createTwoWindingsTransformer(s1, "trf1", "trf1", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
                19, 9, v1.getId(), v2.getId(),
                "trf1", 1, ConnectablePosition.Direction.TOP,
                "trf1", 1, ConnectablePosition.Direction.TOP);
        t2.newRatioTapChanger()
                .setLowTapPosition(0)
                .setTapPosition(1)
                .setLoadTapChangingCapabilities(false)
                .setRegulating(true)
                .setTargetDeadband(1.0)
                .setTargetV(220.0)
                .setRegulationTerminal(t2.getTerminal1())
                .beginStep()
                .setR(39.78473)
                .setX(39.784725)
                .setG(0.0)
                .setB(0.0)
                .setRho(1.0)
                .endStep()
                .beginStep()
                .setR(39.78474)
                .setX(39.784726)
                .setG(0.0)
                .setB(0.0)
                .setRho(1.0)
                .endStep()
                .beginStep()
                .setR(39.78475)
                .setX(39.784727)
                .setG(0.0)
                .setB(0.0)
                .setRho(1.0)
                .endStep()
                .add();

        ThreeWindingsTransformer t3 = createThreeWindingsTransformer(s1, "trf6", "trf6", v1.getId(), v2.getId(), v4.getId(),
                0.5, 0.5, 0.5, 1., 1., 1., 0.1, 0.1,
                400., 225., 225.,
                29, 17, 8,
                "trf61", 5, ConnectablePosition.Direction.TOP,
                "trf62", 5, ConnectablePosition.Direction.TOP,
                "trf63", 3, ConnectablePosition.Direction.TOP);

        t3.getLeg1().newPhaseTapChanger()
                .setTapPosition(1)
                .setLowTapPosition(0)
                .setRegulating(false)
                .setTargetDeadband(1.0)
                .setRegulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .setRegulationValue(10.0)
                .setRegulationTerminal(t3.getLeg1().getTerminal())
                .beginStep()
                .setR(1.0)
                .setX(2.0)
                .setG(3.0)
                .setB(4.0)
                .setAlpha(5.0)
                .setRho(6.0)
                .endStep()
                .beginStep()
                .setR(1.0)
                .setX(2.0)
                .setG(3.0)
                .setB(4.0)
                .setAlpha(5.0)
                .setRho(6.0)
                .endStep()
                .add();

        network.newLine()
                .setId("line1")
                .setVoltageLevel1(v3.getId())
                .setNode1(10)
                .setVoltageLevel2(v4.getId())
                .setNode2(10)
                .setR(1.0)
                .setX(1.0)
                .setG1(1.0)
                .setB1(1.0)
                .setG2(2.0)
                .setB2(2.0)
                .add();

        return network;
    }

    @SuppressWarnings("SameParameterValue")
    private static Substation createSubstation(Network n, String id, String name, Country country) {
        return n.newSubstation()
                .setId(id)
                .setName(name)
                .setCountry(country)
                .add();
    }

    @SuppressWarnings("SameParameterValue")
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
        vl.getNodeBreakerView().newBusbarSection()
                .setId(id)
                .setName(name)
                .setNode(node)
                .add();
    }

    @SuppressWarnings("SameParameterValue")
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

    @SuppressWarnings("SameParameterValue")
    private static void createLoad(VoltageLevel vl, String id, String name,
                                   int node, double p0, double q0) {
        vl.newLoad()
                .setId(id)
                .setName(name)
                .setNode(node)
                .setP0(p0)
                .setQ0(q0)
                .add();
    }

    private static void createLccConverterStation(VoltageLevel vl, String id, String name,
                                                  int node, float powerFactor, float lossFactor) {
        vl.newLccConverterStation()
                .setId(id)
                .setName(name)
                .setNode(node)
                .setLossFactor(lossFactor)
                .setPowerFactor(powerFactor)
                .add();
    }

    @SuppressWarnings("SameParameterValue")
    private static void createGenerator(VoltageLevel vl, String id, int node, double targetP, double targetQ) {
        vl.newGenerator()
                .setId(id)
                .setName(id)
                .setTargetP(targetP)
                .setTargetQ(targetQ)
                .setNode(node)
                .setMinP(-1.1)
                .setMaxP(1000.0)
                .setVoltageRegulatorOn(false)
                .add();
    }

    @SuppressWarnings("SameParameterValue")
    private static TwoWindingsTransformer createTwoWindingsTransformer(Substation s, String id, String name,
                                                                       double r, double x, double g, double b,
                                                                       double ratedU1, double ratedU2,
                                                                       int node1, int node2,
                                                                       String idVoltageLevel1, String idVoltageLevel2,
                                                                       String feederName1, int feederOrder1, ConnectablePosition.Direction direction1,
                                                                       String feederName2, int feederOrder2, ConnectablePosition.Direction direction2) {
        TwoWindingsTransformer t = s.newTwoWindingsTransformer()
                .setId(id)
                .setName(name)
                .setR(r)
                .setX(x)
                .setG(g)
                .setB(b)
                .setRatedU1(ratedU1)
                .setRatedU2(ratedU2)
                .setNode1(node1)
                .setVoltageLevel1(idVoltageLevel1)
                .setNode2(node2)
                .setVoltageLevel2(idVoltageLevel2)
                .add();
        t.newExtension(ConnectablePositionAdder.class)
                .newFeeder1()
                .withName(feederName1)
                .withOrder(feederOrder1)
                .withDirection(direction1).add()
                .newFeeder2()
                .withName(feederName2)
                .withOrder(feederOrder2)
                .withDirection(direction2).add()
                .add();

        return t;
    }

    @SuppressWarnings("SameParameterValue")
    private static ThreeWindingsTransformer createThreeWindingsTransformer(Substation s, String id, String name,
                                                                           String vl1, String vl2, String vl3,
                                                                           double r1, double r2, double r3,
                                                                           double x1, double x2, double x3,
                                                                           double g1, double b1,
                                                                           double ratedU1, double ratedU2, double ratedU3,
                                                                           int node1, int node2, int node3,
                                                                           String feederName1, int feederOrder1, ConnectablePosition.Direction direction1,
                                                                           String feederName2, int feederOrder2, ConnectablePosition.Direction direction2,
                                                                           String feederName3, int feederOrder3, ConnectablePosition.Direction direction3) {
        ThreeWindingsTransformer t = s.newThreeWindingsTransformer()
                .setId(id)
                .setName(name)
                .newLeg1()
                .setR(r1)
                .setX(x1)
                .setG(g1)
                .setB(b1)
                .setRatedU(ratedU1)
                .setVoltageLevel(vl1)
                .setNode(node1)
                .add()
                .newLeg2()
                .setR(r2)
                .setX(x2)
                .setRatedU(ratedU2)
                .setVoltageLevel(vl2)
                .setNode(node2)
                .add()
                .newLeg3()
                .setR(r3)
                .setX(x3)
                .setRatedU(ratedU3)
                .setVoltageLevel(vl3)
                .setNode(node3)
                .add()
                .add();

        t.newExtension(ConnectablePositionAdder.class)
                .newFeeder1()
                .withName(feederName1)
                .withOrder(feederOrder1)
                .withDirection(direction1).add()
                .newFeeder2()
                .withName(feederName2)
                .withOrder(feederOrder2)
                .withDirection(direction2).add()
                .newFeeder3()
                .withName(feederName3)
                .withOrder(feederOrder3)
                .withDirection(direction3).add()
                .add();

        return t;
    }
}
