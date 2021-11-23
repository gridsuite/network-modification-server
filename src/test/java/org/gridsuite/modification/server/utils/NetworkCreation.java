package org.gridsuite.modification.server.utils;

import java.util.UUID;

import com.powsybl.iidm.network.*;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.powsybl.sld.iidm.extensions.ConnectablePosition;
import com.powsybl.sld.iidm.extensions.ConnectablePositionAdder;

public final class NetworkCreation {
    public static final String VARIANT_ID = "variant_1";

    private NetworkCreation() {
    }

    public static Network create(UUID uuid, boolean createHvdcLine) {
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
        createShuntCompensator(v2, "v2shunt", "v2shunt", 7, 225., 10, true, 3, 1, 2, 2);
        createDanglingLine(v2, "v2Dangling", "v2Dangling", 8, 1, 2, 3, 4, 50, 30, "xnode1");
        createVscConverterStation(v2, "v2vsc", "v2vsc", 9, 1, 40, true, 150);

        VoltageLevel v4 = createVoltageLevel(s1, "v4", "v4", TopologyKind.NODE_BREAKER, 380.0);

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v3 = createVoltageLevel(s2, "v3", "v3", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v3, "3A", "3A", 0);
        createSwitch(v3, "v3d1", "v3d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "v3b1", "v3b1", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v3, "v3load", "v3load", 2, 0., 0.);
        createStaticVarCompensator(v3, "v3Compensator", "v3Compensator", 11, StaticVarCompensator.RegulationMode.VOLTAGE, 380., 100, 2, 30);
        createBattery(v3, "v3Battery", "v3Battery", 12, 0, 10, 1, 1);

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

        // create lines
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

        createSwitch(v1, "v1dl1", "v1dl1", SwitchKind.DISCONNECTOR, true, false, false, 0, 4);
        createSwitch(v1, "v1bl1", "v1bl1", SwitchKind.BREAKER, true, false, false, 4, 5);

        createSwitch(v3, "v3dl1", "v3dl1", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v3, "v3bl1", "v3bl1", SwitchKind.BREAKER, true, false, false, 3, 4);

        createLine(network, "line2", "line2", "v1", "v3", 5, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5);

        createSwitch(v1, "v1bl2", "v1bl2", SwitchKind.BREAKER, true, false, true, 0, 6);
        createSwitch(v3, "v3bl2", "v3bl2", SwitchKind.BREAKER, true, false, true, 0, 5);

        createLine(network, "line3", "line3", "v1", "v3", 6, 5, 12.0, 7.0, 5.5, 7.5, 6.5, 8.5);

        if (createHvdcLine) {
            createHvdcLine(network, "hvdcLine", "hvdcLine", 1, 100, HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, 225, 500, "v1lcc", "v2vsc");
        }

        // Creating new variant with new equipments
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, VARIANT_ID);
        network.getVariantManager().setWorkingVariant(VARIANT_ID);

        Substation s1Variant = createSubstation(network, "s1Variant", "s1Variant", Country.FR);
        VoltageLevel v1Variant = createVoltageLevel(s1Variant, "v1Variant", "v1Variant", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1Variant, "bbs1Variant", "bbs1Variant", 0);
        createSwitch(v1Variant, "disc1Variant", "disc1Variant", SwitchKind.DISCONNECTOR, true, true, false, 0, 1);
        createSwitch(v1Variant, "break1Variant", "break1Variant", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v1Variant, "load1Variant", "load1Variant", 2, 0., 0.);

        Substation s2Variant = createSubstation(network, "s2Variant", "s2Variant", Country.FR);
        VoltageLevel v2Variant = createVoltageLevel(s2Variant, "v2Variant", "v2Variant", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v2Variant, "bbs2Variant", "bbs2Variant", 0);
        createSwitch(v2Variant, "disc2Variant", "disc2Variant", SwitchKind.DISCONNECTOR, true, true, false, 0, 1);
        createSwitch(v2Variant, "break2Variant", "break2Variant", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v2Variant, "load2Variant", "load2Variant", 2, 0., 0.);

        createSwitch(v1Variant, "disc11Variant", "disc11Variant", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v1Variant, "break11Variant", "break11Variant", SwitchKind.BREAKER, true, false, false, 3, 4);
        createSwitch(v2Variant, "dsc21Variant", "disc21Variant", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v2Variant, "break21Variant", "break21Variant", SwitchKind.BREAKER, true, false, false, 3, 4);

        createLine(network, "line1Variant", "line1Variant", "v1Variant", "v2Variant", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5);

        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);

        return network;
    }

    public static Network createBusBreaker(UUID uuid) {
        Network network = new NetworkFactoryImpl().createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.BUS_BREAKER, 380.0);
        VoltageLevel v12 = createVoltageLevel(s1, "v12", "v12", TopologyKind.BUS_BREAKER, 90.0);
        createBus(v1, "bus1", "bus1");
        createBus(v12, "bus12", "bus12");
        createGeneratorOnBus(v1, "idGenerator1", "bus1", 42.1, 1.0);

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", "v2", TopologyKind.BUS_BREAKER, 225.0);
        createBus(v2, "bus2", "bus2");

        return network;
    }

    public static Network createMixedTopology(UUID uuid) {
        Network network = new NetworkFactoryImpl().createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1, "1.1", "1.1", 0);
        createSwitch(v1, "v1d1", "v1d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "v1b1", "v1b1", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v1, "v1load", "v1load", 2, 0., 0.);
        createLccConverterStation(v1, "v1lcc", "v1lcc", 3, 0, 0);
        VoltageLevel v3 = createVoltageLevel(s1, "v3", "v3", TopologyKind.BUS_BREAKER, 450.0);
        createBus(v3, "bus3", "bus3");

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", "v2", TopologyKind.BUS_BREAKER, 225.0);
        createBus(v2, "bus2", "bus2");

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

    private static void createBus(VoltageLevel vl, String id, String name) {
        vl.getBusBreakerView().newBus()
            .setId(id)
            .setName(name)
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

    private static void createLine(Network network, String id, String name, String voltageLevel1, String voltageLevel2, int node1, int node2,
                                   double r, double x, double g1, double g2, double b1, double b2) {
        network.newLine()
            .setId(id)
            .setName(name)
            .setR(r)
            .setX(x)
            .setG1(g1)
            .setG2(g2)
            .setB1(b1)
            .setB2(b2)
            .setVoltageLevel1(voltageLevel1)
            .setVoltageLevel2(voltageLevel2)
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

    private static void createGeneratorOnBus(VoltageLevel vl, String id, String busId, double targetP, double targetQ) {
        vl.newGenerator()
            .setId(id)
            .setName(id)
            .setTargetP(targetP)
            .setTargetQ(targetQ)
            .setBus(busId)
            .setConnectableBus(busId)
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

    private static void createShuntCompensator(VoltageLevel vl, String id, String name,
                                               int node, double targetV, double targetDeadband, boolean voltageRegulatorOn,
                                               int maximumSectionCount, double bPerSection, double gPerSection, int sectionCount) {
        vl.newShuntCompensator()
            .setId(id)
            .setName(name)
            .setNode(node)
            .setTargetV(targetV)
            .setTargetDeadband(targetDeadband)
            .setVoltageRegulatorOn(voltageRegulatorOn)
            .newLinearModel()
            .setMaximumSectionCount(maximumSectionCount)
            .setBPerSection(bPerSection)
            .setGPerSection(gPerSection)
            .add()
            .setSectionCount(sectionCount)
            .add();
    }

    private static void createStaticVarCompensator(VoltageLevel vl, String id, String name,
                                                   int node, StaticVarCompensator.RegulationMode regulationMode, double voltageSetpoint,
                                                   double reactivePowerSetpoint, double bMin, double bMax) {
        vl.newStaticVarCompensator()
            .setId(id)
            .setName(name)
            .setRegulationMode(regulationMode)
            .setVoltageSetpoint(voltageSetpoint)
            .setReactivePowerSetpoint(reactivePowerSetpoint)
            .setBmin(bMin)
            .setBmax(bMax)
            .setNode(node)
            .add();
    }

    private static void createBattery(VoltageLevel vl, String id, String name,
                                      int node, double minP, double maxP, double p0, double q0) {
        vl.newBattery()
            .setId(id)
            .setName(name)
            .setMinP(minP)
            .setMaxP(maxP)
            .setP0(p0)
            .setQ0(q0)
            .setNode(node)
            .add();
    }

    private static void createDanglingLine(VoltageLevel vl, String id, String name,
                                           int node, double r, double x, double b, double g, double p0, double q0, String ucteXnodeCode) {
        vl.newDanglingLine()
            .setId(id)
            .setName(name)
            .setR(r)
            .setX(x)
            .setB(b)
            .setG(g)
            .setP0(p0)
            .setQ0(q0)
            .setUcteXnodeCode(ucteXnodeCode)
            .setNode(node)
            .add();
    }

    private static void createVscConverterStation(VoltageLevel vl, String id, String name,
                                                  int node, float lossFactor,
                                                  double reactivePowerSetpoint, boolean voltageRegulatorOn, double voltageSetpoint) {
        vl.newVscConverterStation()
            .setId(id)
            .setName(name)
            .setNode(node)
            .setLossFactor(lossFactor)
            .setReactivePowerSetpoint(reactivePowerSetpoint)
            .setVoltageRegulatorOn(voltageRegulatorOn)
            .setVoltageSetpoint(voltageSetpoint)
            .add();
    }

    private static void createHvdcLine(Network network, String id, String name,
                                       double r, double maxP, HvdcLine.ConvertersMode hvdcLineConvertersMode,
                                       double nominalV, double activePowerSetpoint,
                                       String converterStationId1, String converterStationId2) {
        network.newHvdcLine()
            .setId(id)
            .setName(name)
            .setR(r)
            .setMaxP(maxP)
            .setConvertersMode(hvdcLineConvertersMode)
            .setNominalV(nominalV)
            .setActivePowerSetpoint(activePowerSetpoint)
            .setConverterStationId1(converterStationId1)
            .setConverterStationId2(converterStationId2)
            .add();
    }
}
