/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.server.modifications.ModificationUtils;

import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.*;

public final class NetworkCreation {
    public static final String VARIANT_ID = "variant_1";

    private NetworkCreation() {
        throw new IllegalCallerException("Utility class");
    }

    public static Network create(UUID uuid, boolean createHvdcLine) {
        return create(uuid, createHvdcLine, new NetworkFactoryImpl());
    }

    //Create a network with fictitious switch and different switch kind
    public static Network createSwitchNetwork(UUID uuid, NetworkFactory networkFactory) {
        Network network = networkFactory.createNetwork(uuid.toString(), "test");

        Substation s1 = network.newSubstation()
                .setId("s1")
                .setName("s1")
                .add();
        VoltageLevel vl1 = createVoltageLevel(s1, "vl1", "vl1", TopologyKind.NODE_BREAKER, 400.0);
        vl1.getNodeBreakerView().newBusbarSection()
                .setId("b1")
                .setName("b1")
                .setNode(0)
                .add();

        VoltageLevel vl2 = createVoltageLevel(s1, "vl2", "vl2", TopologyKind.NODE_BREAKER, 400.0);
        vl2.getNodeBreakerView().newBusbarSection()
                .setId("b2")
                .setName("b2")
                .setNode(0)
                .add();

        createSwitch(vl1, "b4", "b4", SwitchKind.DISCONNECTOR, false, false, false, 0, 1);
        createSwitch(vl1, "br11", "br11", SwitchKind.BREAKER, false, false, false, 1, 2);
        createSwitch(vl2, "b5", "b5", SwitchKind.DISCONNECTOR, false, false, false, 0, 1);
        createSwitch(vl2, "br21", "br21", SwitchKind.BREAKER, false, false, true, 1, 2);

        network.newLine()
                .setId("line2")
                .setName("line2")
                .setVoltageLevel1("vl1")
                .setVoltageLevel2("vl2")
                .setR(0.1)
                .setX(10.0)
                .setG1(0.0)
                .setG2(0.0)
                .setB1(0.0)
                .setB2(0.0)
                .setNode1(2)
                .setNode2(2)
                .add();

        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);
        return network;
    }

    public static Network create(UUID uuid, boolean createHvdcLine, NetworkFactory networkFactory) {
        Network network = networkFactory.createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1, "1.1", "1.1", 0);

        createLoad(v1, "v1load", "v1load", 2, 0., 0., "cn1", 0, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v1, "v1d1", "v1d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "v1b1", "v1b1", SwitchKind.BREAKER, true, false, false, 1, 2);

        createLccConverterStation(v1, "v1lcc", "v1lcc", 11, 0, 0);
        createSwitch(v1, "v1dlcc", "v1dlcc", SwitchKind.DISCONNECTOR, true, false, false, 0, 12);
        createSwitch(v1, "v1blcc", "v1blcc", SwitchKind.BREAKER, true, false, false, 12, 11);

        VoltageLevel v2 = createVoltageLevel(s1, "v2", "v2", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v2, "1A", "1A", 0);
        createBusBarSection(v2, "1B", "1B", 1);
        createSwitch(v2, "v2d1", "v2d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v2, "v2b1", "v2b1", SwitchKind.BREAKER, true, true, false, 2, 3);
        createSwitch(v2, "v2d2", "v2d2", SwitchKind.DISCONNECTOR, true, false, false, 3, 1);

        createLoad(v2, "v2load", "v2load", 5, 0., 0., "cn2", 2, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v2, "v2dload", "v2dload", SwitchKind.DISCONNECTOR, true, false, false, 1, 4);
        createSwitch(v2, "v2bload", "v2bload", SwitchKind.BREAKER, true, false, false, 4, 5);

        createGenerator(v2, "idGenerator", 6, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "v2bgenerator", "v2bgenerator", SwitchKind.BREAKER, true, false, false, 6, 7);
        createSwitch(v2, "v2dgenerator", "v2dgenerator", SwitchKind.DISCONNECTOR, true, false, false, 7, 1);

        createShuntCompensator(v2, "v2shunt", "v2shunt", 8, 225., 10, true, 3, 1, 2, 2, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v2, "v2bshunt", "v2bshunt", SwitchKind.BREAKER, true, false, false, 8, 9);
        createSwitch(v2, "v2dshunt", "v2dshunt", SwitchKind.DISCONNECTOR, true, false, false, 9, 0);

        createDanglingLine(v2, "v2Dangling", "v2Dangling", 10, 1, 2, 3, 4, 50, 30, "xnode1");
        createSwitch(v2, "v2bdangling", "v2bdangling", SwitchKind.BREAKER, true, false, false, 10, 11);
        createSwitch(v2, "v2ddangling", "v2ddangling", SwitchKind.DISCONNECTOR, true, false, false, 11, 1);

        createVscConverterStation(v2, "v2vsc", "v2vsc", 12, 1, 40, true, 150);
        createSwitch(v2, "v2bvsc", "v2bdvsc", SwitchKind.BREAKER, true, false, false, 12, 13);
        createSwitch(v2, "v2dvsc", "v2dvsc", SwitchKind.DISCONNECTOR, true, false, false, 13, 0);

        VoltageLevel v4 = createVoltageLevel(s1, "v4", "v4", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v4, "1.A", "1.A", 0);

        Substation s3 = createSubstation(network, "s3", "s3", Country.FR);
        s3.setProperty("tso", "rtefrance");
        s3.setProperty("region", "west");
        VoltageLevel v5 = createVoltageLevel(s3, "v5", "v5", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v5, "1A1", "1A1", 0);
        createLoad(v5, "v5load", "v5load", 2, 0., 0., "cn5", 5, ConnectablePosition.Direction.TOP);
        createShuntCompensator(v5, "v5shunt", "v5shunt", 4, 225., 10, true, 3, 1, 2, 2, "cn22", 33, ConnectablePosition.Direction.BOTTOM);
        createGenerator(v5, "v5generator", 3, 42.1, 1.0, "cn10", 10, ConnectablePosition.Direction.TOP);
        createStaticVarCompensator(v5, "v5Compensator", "v5Compensator", 5, StaticVarCompensator.RegulationMode.VOLTAGE, 380., 100, 2, 30);

        VoltageLevel v6 = createVoltageLevel(s3, "v6", "v6", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v6, "1B1", "1B1", 0);
        createLoad(v6, "v6load", "v6load", 2, 0., 0., "cn6", 6, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(v6, "v6shunt", "v6shunt", 4, 225., 10, true, 3, 1, 2, 2, "cn33", 44, ConnectablePosition.Direction.BOTTOM);
        createGenerator(v6, "v6generator", 3, 42.1, 1.0, "cn11", 11, ConnectablePosition.Direction.TOP);
        createStaticVarCompensator(v6, "v6Compensator", "v6Compensator", 5, StaticVarCompensator.RegulationMode.VOLTAGE, 380., 100, 2, 30);

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v3 = createVoltageLevel(s2, "v3", "v3", TopologyKind.NODE_BREAKER, 380.0, 15.0, 25.0);
        createBusBarSection(v3, "3A", "3A", 0);

        createLoad(v3, "v3load", "v3load", 2, 0., 0., "cn3", 3, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v3, "v3d1", "v3d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "v3b1", "v3b1", SwitchKind.BREAKER, true, false, false, 1, 2);

        createStaticVarCompensator(v3, "v3Compensator", "v3Compensator", 4, StaticVarCompensator.RegulationMode.VOLTAGE, 380., 100, 2, 30);
        createSwitch(v3, "v3dCompensator", "v3dCompensator", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v3, "v3bCompensator", "v3bCompensator", SwitchKind.BREAKER, true, false, false, 3, 4);

        createBattery(v1, "v1Battery", "v1Battery", 80, 0, 15, 6, 3);
        createSwitch(v1, "v1dBattery", "v1dBattery", SwitchKind.DISCONNECTOR, true, false, false, 0, 81);
        createSwitch(v1, "v1bBattery", "v1bBattery", SwitchKind.BREAKER, true, false, false, 81, 80);

        createBattery(v2, "v2Battery", "v2Battery", 50, 0, 20, 7, 11);
        createSwitch(v2, "v2dBattery", "v2dBattery", SwitchKind.DISCONNECTOR, true, false, false, 0, 51);
        createSwitch(v2, "v2bBattery", "v2bBattery", SwitchKind.BREAKER, true, false, false, 51, 50);

        createBattery(v3, "v3Battery", "v3Battery", 6, 0, 10, 1, 1);
        createSwitch(v3, "v3dBattery", "v3dBattery", SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "v3bBattery", "v3bBattery", SwitchKind.BREAKER, true, false, false, 5, 6);

        TwoWindingsTransformer t2 = createTwoWindingsTransformer(s1, "trf1", "trf1", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            4, 14, v1.getId(), v2.getId(),
            "trf1", 1, ConnectablePosition.Direction.TOP,
            "trf1", 2, ConnectablePosition.Direction.TOP);
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

        createSwitch(v1, "v1btrf1", "v1btrf1", SwitchKind.BREAKER, true, false, false, 4, 5);
        createSwitch(v1, "v1dtrf1", "v1dtrf1", SwitchKind.DISCONNECTOR, true, false, false, 5, 0);
        createSwitch(v2, "v2btrf1", "v2btrf1", SwitchKind.BREAKER, true, false, false, 14, 15);
        createSwitch(v2, "v2dtrf1", "v2dtrf1", SwitchKind.DISCONNECTOR, true, false, false, 15, 1);

        TwoWindingsTransformer twt2 = createTwoWindingsTransformer(s1, "trf2", "trf2", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            40, 150, v1.getId(), v2.getId(),
            "trf2", 1, ConnectablePosition.Direction.TOP,
            "trf2", 2, ConnectablePosition.Direction.TOP);
        Terminal phaseTapChangerTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
            "v3load",
            "LOAD",
            "V3");
        twt2.newPhaseTapChanger()
            .setLowTapPosition(0)
            .setTapPosition(1)
            .setRegulationTerminal(phaseTapChangerTerminal)
            .setRegulationMode(PhaseTapChanger.RegulationMode.FIXED_TAP)
            .setTargetDeadband(2.)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.1)
            .endStep()
            .add();

        createSwitch(v1, "v1btrf2", "v1btrf2", SwitchKind.BREAKER, true, false, false, 40, 50);
        createSwitch(v1, "v1dtrf2", "v1dtrf2", SwitchKind.DISCONNECTOR, true, false, false, 50, 0);
        createSwitch(v2, "v2dtrf2", "v2dtrf2", SwitchKind.DISCONNECTOR, true, false, false, 150, 1);

        ThreeWindingsTransformer t3 = createThreeWindingsTransformer(s1, "trf6", "trf6", v1.getId(), v2.getId(), v4.getId(),
            0.5, 0.5, 0.5, 1., 1., 1., 0.1, 0.1,
            400., 225., 225.,
            51, 16, 1,
            "trf61", 5, ConnectablePosition.Direction.TOP,
            "trf62", 5, ConnectablePosition.Direction.TOP,
            "trf63", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "v1btrf6", "v1btrf6", SwitchKind.BREAKER, true, false, false, 51, 6);
        createSwitch(v1, "v1dtrf6", "v1dtrf6", SwitchKind.DISCONNECTOR, true, false, false, 6, 0);
        createSwitch(v2, "v2btrf6", "v2btrf6", SwitchKind.BREAKER, true, false, false, 16, 17);
        createSwitch(v2, "v2dtrf6", "v2dtrf6", SwitchKind.DISCONNECTOR, true, false, false, 17, 0);
        createSwitch(v4, "v4btrf6", "v4btrf6", SwitchKind.BREAKER, true, false, false, 1, 2);
        createSwitch(v4, "v4dtrf6", "v4dtrf6", SwitchKind.DISCONNECTOR, true, false, false, 2, 0);

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
        createLine(network, "line1", "line1", "v3", "v4", 8, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "cn1line1", 1, ConnectablePosition.Direction.TOP, "cn2line1", 1, ConnectablePosition.Direction.TOP);
        createSwitch(v3, "v3dl1", "v3dl1", SwitchKind.DISCONNECTOR, true, false, false, 0, 7);
        createSwitch(v3, "v3bl1", "v3bl1", SwitchKind.BREAKER, true, false, false, 7, 8);
        createSwitch(v4, "v4dl1", "v4dl1", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v4, "v4bl1", "v4bl1", SwitchKind.BREAKER, true, false, false, 3, 4);

        createLine(network, "line2", "line2", "v1", "v3", 31, 31, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "cn1line2", 2, ConnectablePosition.Direction.TOP, "cn2line2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "v1dl2", "v1dl2", SwitchKind.DISCONNECTOR, true, false, false, 0, 30);
        createSwitch(v1, "v1bl2", "v1bl2", SwitchKind.BREAKER, true, false, false, 30, 31);
        createSwitch(v3, "v3dl2", "v3dl2", SwitchKind.DISCONNECTOR, true, false, false, 0, 30);
        createSwitch(v3, "v3bl2", "v3bl2", SwitchKind.BREAKER, true, false, false, 30, 31);

        createLineWithoutConnectivity(network, "line3", "line3", "v1", "v3", 10, 12, 12.0, 7.0, 5.5, 7.5, 6.5, 8.5);
        createSwitch(v1, "v1dl3", "v1dl3", SwitchKind.DISCONNECTOR, true, false, false, 0, 9);
        createSwitch(v1, "v1bl3", "v1bl3", SwitchKind.BREAKER, true, false, true, 9, 10);
        createSwitch(v3, "v3dl3", "v3dl3", SwitchKind.DISCONNECTOR, true, false, false, 0, 11);
        createSwitch(v3, "v3bl3", "v3bl3", SwitchKind.BREAKER, true, false, true, 11, 12);

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
        createLoad(v1Variant, "load1Variant", "load1Variant", 2, 0., 0., "cn1", 0, ConnectablePosition.Direction.BOTTOM);

        Substation s2Variant = createSubstation(network, "s2Variant", "s2Variant", Country.FR);
        VoltageLevel v2Variant = createVoltageLevel(s2Variant, "v2Variant", "v2Variant", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v2Variant, "bbs2Variant", "bbs2Variant", 0);
        createSwitch(v2Variant, "disc2Variant", "disc2Variant", SwitchKind.DISCONNECTOR, true, true, false, 0, 1);
        createSwitch(v2Variant, "break2Variant", "break2Variant", SwitchKind.BREAKER, true, false, false, 1, 2);
        createLoad(v2Variant, "load2Variant", "load2Variant", 2, 0., 0., "cn1", 0, ConnectablePosition.Direction.BOTTOM);

        createSwitch(v1Variant, "disc11Variant", "disc11Variant", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v1Variant, "break11Variant", "break11Variant", SwitchKind.BREAKER, true, false, false, 3, 4);
        createSwitch(v2Variant, "dsc21Variant", "disc21Variant", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
        createSwitch(v2Variant, "break21Variant", "break21Variant", SwitchKind.BREAKER, true, false, false, 3, 4);

        createLine(network, "line1Variant", "line1Variant", "v1Variant", "v2Variant", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "cn1line1Variant", 1, ConnectablePosition.Direction.TOP, "cn2line1Variant", 1, ConnectablePosition.Direction.TOP);

        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);

        return network;
    }

    public static Network createGeneratorsNetwork(UUID uuid, NetworkFactory networkFactory) {
        Network network = networkFactory.createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1, "1.1", "1.1", 0);
        createBusBarSection(v1, "1.2", "1.2", 1);
        createGenerator(v1, "gen1", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "v1d1", "v1d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);

        VoltageLevel v2 = createVoltageLevel(s1, "v2", "v2", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v2, "2.1", "2.1", 0);
        createBusBarSection(v2, "2.2", "2.2", 1);
        createGenerator(v2, "gen2", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createGenerator(v2, "gen3", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "v2d1", "v2d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v2, "v2d2", "v2d2", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v21 = createVoltageLevel(s2, "v21", "v21", TopologyKind.NODE_BREAKER, 450.0);
        createBusBarSection(v21, "3.1", "3.1", 0);
        createBusBarSection(v21, "3.2", "3.2", 1);
        createGenerator(v21, "gen4", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v21, "v21d1", "v21d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);

        VoltageLevel v22 = createVoltageLevel(s2, "v22", "v22", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v22, "4.1", "4.1", 0);
        createBusBarSection(v22, "4.2", "4.2", 1);
        createGenerator(v22, "gen5", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createGenerator(v22, "gen6", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v22, "v22d1", "v22d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v22, "v22d2", "v22d2", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);

        Substation s3 = createSubstation(network, "s3", "s3", Country.FR);
        VoltageLevel v31 = createVoltageLevel(s3, "v31", "v31", TopologyKind.NODE_BREAKER, 450.0);
        createBusBarSection(v31, "5.1", "5.1", 0);
        createBusBarSection(v31, "5.2", "5.2", 1);
        createGenerator(v31, "gen7", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createGenerator(v31, "gen8", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v31, "v31d1", "v31d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v31, "v31d2", "v31d2", SwitchKind.DISCONNECTOR, true, false, false, 1, 3);

        VoltageLevel v32 = createVoltageLevel(s3, "v32", "v32", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v32, "6.1", "6.1", 0);
        createBusBarSection(v32, "6.2", "6.2", 1);
        createGenerator(v32, "gen9", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createGenerator(v32, "gen10", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v32, "v32d1", "v32d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v32, "v32d2", "v32d2", SwitchKind.DISCONNECTOR, true, false, false, 1, 3);

        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, VARIANT_ID);

        return network;
    }

    public static Network createLoadNetwork(UUID uuid, NetworkFactory networkFactory) {
        Network network = networkFactory.createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v1, "1.1", "1.1", 0);
        createBusBarSection(v1, "1.2", "1.2", 1);
        createLoad(v1, "load1", "load1", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "v1d1", "v1d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);

        VoltageLevel v2 = createVoltageLevel(s1, "v2", "v2", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v2, "2.1", "2.1", 0);
        createBusBarSection(v2, "2.2", "2.2", 1);
        createLoad(v2, "load2", "load2", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createLoad(v2, "load3", "load3", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "v2d1", "v2d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v2, "v2d2", "v2d2", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);

        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v21 = createVoltageLevel(s2, "v21", "v21", TopologyKind.NODE_BREAKER, 450.0);
        createBusBarSection(v21, "3.1", "3.1", 0);
        createBusBarSection(v21, "3.2", "3.2", 1);
        createLoad(v21, "load4", "load4", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v21, "v21d1", "v21d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);

        VoltageLevel v22 = createVoltageLevel(s2, "v22", "v22", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v22, "4.1", "4.1", 0);
        createBusBarSection(v22, "4.2", "4.2", 1);
        createLoad(v22, "load5", "load5", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createLoad(v22, "load6", "load6", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v22, "v22d1", "v22d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v22, "v22d2", "v22d2", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);

        Substation s3 = createSubstation(network, "s3", "s3", Country.FR);
        VoltageLevel v31 = createVoltageLevel(s3, "v31", "v31", TopologyKind.NODE_BREAKER, 450.0);
        createBusBarSection(v31, "5.1", "5.1", 0);
        createBusBarSection(v31, "5.2", "5.2", 1);
        createLoad(v31, "load7", "load7", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createLoad(v31, "load8", "load8", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v31, "v31d1", "v31d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v31, "v31d2", "v31d2", SwitchKind.DISCONNECTOR, true, false, false, 1, 3);

        VoltageLevel v32 = createVoltageLevel(s3, "v32", "v32", TopologyKind.NODE_BREAKER, 225.0);
        createBusBarSection(v32, "6.1", "6.1", 0);
        createBusBarSection(v32, "6.2", "6.2", 1);
        createLoad(v32, "load9", "load9", 2, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createLoad(v32, "load10", "load10", 3, 42.1, 1.0, "cn0", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v32, "v32d1", "v32d1", SwitchKind.DISCONNECTOR, true, false, false, 0, 2);
        createSwitch(v32, "v32d2", "v32d2", SwitchKind.DISCONNECTOR, true, false, false, 1, 3);

        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, VARIANT_ID);

        return network;
    }

    public static Network createBusBreaker(UUID uuid) {
        Network network = new NetworkFactoryImpl().createNetwork(uuid.toString(), "test");

        Substation s1 = createSubstation(network, "s1", "s1", Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", "v1", TopologyKind.BUS_BREAKER, 380.0);
        VoltageLevel v12 = createVoltageLevel(s1, "v12", "v12", TopologyKind.BUS_BREAKER, 90.0);
        createBus(v1, "bus1", "bus1");
        createGeneratorOnBus(v1, "idGenerator1", "bus1", 42.1, 1.0);
        createBus(v12, "bus12", "bus12");
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
        createLoad(v1, "v1load", "v1load", 2, 0., 0., "cn1", 0, ConnectablePosition.Direction.BOTTOM);
        createLccConverterStation(v1, "v1lcc", "v1lcc", 3, 0, 0);
        VoltageLevel v3 = createVoltageLevel(s1, "v3", "v3", TopologyKind.BUS_BREAKER, 450.0);
        createBus(v3, "bus3", "bus3");
        Substation s2 = createSubstation(network, "s2", "s2", Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", "v2", TopologyKind.BUS_BREAKER, 225.0);
        createBus(v2, "bus2", "bus2");

        return network;
    }

    /**
     * Create a network as following:
     * <pre>
     *     VL1            VL2             VL3
     *
     *     ld1            g2              ld3
     *      |              |               |
     *     br1            br2             br3
     *      |              |               |
     *     d1             d2              d3
     *      |              |               |
     *     bbs1 ----------bbs2------------bbs3
     *              l1             l2
     * </pre>
     */
    public static Network createForDeleteVoltageLevelOnLine(UUID uuid) {
        Network network = new NetworkFactoryImpl().createNetwork(uuid.toString(), "NetworkForDeleteVoltageLevelOnLine");

        // VL1
        Substation s1 = createSubstation(network, "s1", null, Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v1, "bbs1", null, 0);
        createLoad(v1, "ld1", null, 2, 0., 0., "ld1", 0, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v1, "d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "br1", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL2
        Substation s2 = createSubstation(network, "s2", null, Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v2, "bbs2", null, 0);

        createGenerator(v2, "g2", 2, 42.1, 1.0, "g2", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v2, "br2", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL3
        Substation s3 = createSubstation(network, "s3", null, Country.FR);
        VoltageLevel v3 = createVoltageLevel(s3, "v3", null, TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v3, "bbs3", null, 0);

        createLoad(v3, "ld3", null, 2, 0., 0., "ld3", 3, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v3, "d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "br3", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // create lines
        createLine(network, "l1", null, "v1", "v2", 4, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "l1", 1, ConnectablePosition.Direction.TOP, "l1", 1, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l1d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l1br1", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v2, "l1d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l1br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l2", null, "v1", "v3", 6, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "l2", 2, ConnectablePosition.Direction.TOP, "l2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l2d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 7);
        createSwitch(v1, "l2br1", null, SwitchKind.BREAKER, true, false, false, 7, 6);
        createSwitch(v3, "l2d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "l2br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        return network;
    }

    public static Network createWithVSC(UUID uuid, boolean withAngleDropExtention) {
        NetworkFactoryImpl networkFactory = new NetworkFactoryImpl();
        Network network = create(uuid, false, networkFactory);
        VoltageLevel v1 = network.getVoltageLevel("v1");

        createVscConverterStation(v1, "v1vsc", "v1vsc", 18, 1, 40, true, 150);
        createSwitch(v1, "v1dvsc", "v1dvsc", SwitchKind.DISCONNECTOR, true, false, false, 0, 12);
        createSwitch(v1, "v1bvsc", "v1bvsc", SwitchKind.BREAKER, true, false, false, 12, 18);

        createHvdcLine(network, "hvdcLine", "hvdcLine", 1, 100, HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, 225, 500, "v1vsc", "v2vsc");
        HvdcLine hvdcLine = network.getHvdcLine("hvdcLine");
        if (withAngleDropExtention) {
            hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class)
                    .withEnabled(true)
                    .withP0(0F)
                    .withDroop(10F)
                    .add();
        }

        return network;
    }
}
