/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;

public final class NetworkUtil {
    private NetworkUtil() {
        throw new IllegalStateException("Utility class");
    }

    @SuppressWarnings("SameParameterValue")
    public static Substation createSubstation(Network n, String id, String name, Country country) {
        return n.newSubstation()
            .setId(id)
            .setName(name)
            .setCountry(country)
            .add();
    }

    @SuppressWarnings("SameParameterValue")
    public static VoltageLevel createVoltageLevel(Substation s, String id, String name,
                                                   TopologyKind topology, double vNom) {
        return s.newVoltageLevel()
            .setId(id)
            .setName(name)
            .setTopologyKind(topology)
            .setNominalV(vNom)
            .add();
    }

    public static VoltageLevel createVoltageLevel(Substation s, String id, String name,
                                                  TopologyKind topology, double vNom, double ipMin, double ipMax) {
        VoltageLevel vl = createVoltageLevel(s, id, name, topology, vNom);
        vl.newExtension(IdentifiableShortCircuitAdder.class).withIpMin(ipMin).withIpMax(ipMax).add();
        return vl;
    }

    public static void createBusBarSection(VoltageLevel vl, String id, String name, int node) {
        var bbs = vl.getNodeBreakerView().newBusbarSection()
            .setId(id)
            .setName(name)
            .setNode(node)
            .add();
        bbs.newExtension(BusbarSectionPositionAdder.class).withBusbarIndex(0).withSectionIndex(0).add();
    }

    public static void createBus(VoltageLevel vl, String id, String name) {
        vl.getBusBreakerView().newBus()
            .setId(id)
            .setName(name)
            .add();
    }

    @SuppressWarnings("SameParameterValue")
    public static void createSwitch(VoltageLevel vl, String id, String name, SwitchKind kind, boolean retained, boolean open, boolean fictitious, int node1, int node2) {
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

    public static void createLine(Network network, String id, String name, String voltageLevel1, String voltageLevel2, int node1, int node2,
                                   double r, double x, double g1, double g2, double b1, double b2,
                                   String feederName1, int feederOrder1, ConnectablePosition.Direction direction1,
                                   String feederName2, int feederOrder2, ConnectablePosition.Direction direction2) {
        var l = network.newLine()
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

        l.newExtension(ConnectablePositionAdder.class)
                .newFeeder1()
                .withName(feederName1)
                .withOrder(feederOrder1)
                .withDirection(direction1).add()
                .newFeeder2()
                .withName(feederName2)
                .withOrder(feederOrder2)
                .withDirection(direction2).add()
                .add();
    }

    public static void createLineWithoutConnectivity(Network network, String id, String name, String voltageLevel1, String voltageLevel2, int node1, int node2,
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
    public static void createLoad(VoltageLevel vl, String id, String name,
                                   int node, double p0, double q0, String feederName, int feederOrder, ConnectablePosition.Direction direction) {
        var l = vl.newLoad()
            .setId(id)
            .setName(name)
            .setNode(node)
            .setP0(p0)
            .setQ0(q0)
            .add();
        l.newExtension(ConnectablePositionAdder.class)
                .newFeeder()
                .withName(feederName)
                .withOrder(feederOrder)
                .withDirection(direction)
                .add();
    }

    public static void createLccConverterStation(VoltageLevel vl, String id, String name,
                                                  int node, float powerFactor, float lossFactor) {
        vl.newLccConverterStation()
            .setId(id)
            .setName(name)
            .setNode(node)
            .setLossFactor(lossFactor)
            .setPowerFactor(powerFactor)
            .add();
    }

    public static void createGenerator(VoltageLevel vl, String id, int node, double targetP, double targetQ, String feederName, int feederOrder, ConnectablePosition.Direction direction) {
        createGenerator(vl, id, node, targetP, targetQ, feederName, feederOrder, direction, 1000.0, -1.1);
    }

    @SuppressWarnings("SameParameterValue")
    public static void createGenerator(VoltageLevel vl, String id, int node, double targetP, double targetQ, String feederName, int feederOrder, ConnectablePosition.Direction direction, double maxP, double minP) {
        var g = vl.newGenerator()
            .setId(id)
            .setName(id)
            .setTargetP(targetP)
            .setTargetQ(targetQ)
            .setNode(node)
            .setMinP(minP)
            .setMaxP(maxP)
            .setVoltageRegulatorOn(false)
            .add();

        g.newExtension(ConnectablePositionAdder.class)
                .newFeeder()
                .withName(feederName)
                .withOrder(feederOrder)
                .withDirection(direction).add();
    }

    public static void createGeneratorOnBus(VoltageLevel vl, String id, String busId, double targetP, double targetQ) {
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
    public static TwoWindingsTransformer createTwoWindingsTransformer(Substation s, String id, String name,
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
    public static ThreeWindingsTransformer createThreeWindingsTransformer(Substation s, String id, String name,
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

    public static void createShuntCompensator(VoltageLevel vl, String id, String name,
                                               int node, double targetV, double targetDeadband, boolean voltageRegulatorOn,
                                               int maximumSectionCount, double bPerSection, double gPerSection, int sectionCount, String feederName, int feederOrder, ConnectablePosition.Direction direction) {
        var sh = vl.newShuntCompensator()
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
        sh.newExtension(ConnectablePositionAdder.class)
                .newFeeder()
                .withName(feederName)
                .withOrder(feederOrder)
                .withDirection(direction).add();
    }

    public static void createStaticVarCompensator(VoltageLevel vl, String id, String name,
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

    public static void createBattery(VoltageLevel vl, String id, String name,
                                      int node, double minP, double maxP, double targetP, double targetQ) {
        vl.newBattery()
            .setId(id)
            .setName(name)
            .setMinP(minP)
            .setMaxP(maxP)
            .setTargetP(targetP)
            .setTargetQ(targetQ)
            .setNode(node)
            .add();
    }

    public static void createDanglingLine(VoltageLevel vl, String id, String name,
                                           int node, double r, double x, double b, double g, double p0, double q0, String pairingKey) {
        vl.newDanglingLine()
            .setId(id)
            .setName(name)
            .setR(r)
            .setX(x)
            .setB(b)
            .setG(g)
            .setP0(p0)
            .setQ0(q0)
            .setPairingKey(pairingKey)
            .setNode(node)
            .add();
    }

    public static void createVscConverterStation(VoltageLevel vl, String id, String name,
                                                  int node, float lossFactor,
                                                  double reactivePowerSetpoint, boolean voltageRegulatorOn, double voltageSetpoint) {
        VscConverterStation vsc = vl.newVscConverterStation()
            .setId(id)
            .setName(name)
            .setNode(node)
            .setLossFactor(lossFactor)
            .setReactivePowerSetpoint(reactivePowerSetpoint)
            .setVoltageRegulatorOn(voltageRegulatorOn)
            .setVoltageSetpoint(voltageSetpoint)
            .add();

        vsc.newMinMaxReactiveLimits().setMinQ(0).setMaxQ(10).add();
    }

    public static void createHvdcLine(Network network, String id, String name,
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
