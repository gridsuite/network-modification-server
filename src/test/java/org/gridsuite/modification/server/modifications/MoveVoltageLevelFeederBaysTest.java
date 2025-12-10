/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.dto.MoveFeederBayInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.MoveVoltageLevelFeederBaysInfos;
import org.gridsuite.modification.modifications.MoveVoltageLevelFeederBays;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.utils.ModificationUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.createBusBarSection;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@Tag("IntegrationTest")
class MoveVoltageLevelFeederBaysTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        createBusBarSection(network.getVoltageLevel("v3"), "3B", "3B", 20);
        createBusBarSection(network.getVoltageLevel("v3"), "3C", "3C", 30);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        List<MoveFeederBayInfos> moveFeederBayInfos = new ArrayList<>();
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("v3load")
            .busbarSectionId("3A")
            .connectionSide(null)
            .connectionName("v3loadrename")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("line2")
            .busbarSectionId("3C")
            .connectionSide(ThreeSides.TWO.toString())
            .connectionName("line2NameV3")
            .connectionPosition(10)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build());
        return MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBays(moveFeederBayInfos)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<MoveFeederBayInfos> moveFeederBayInfos = new ArrayList<>();
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("v3load")
            .busbarSectionId("3A")
            .connectionSide(null)
            .connectionName("v3loadmodified")
            .connectionPosition(6)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build());
        moveFeederBayInfos.add(MoveFeederBayInfos.builder()
            .equipmentId("line2")
            .busbarSectionId("3B")
            .connectionSide(ThreeSides.ONE.toString())
            .connectionName("line2modifiedbis")
            .connectionPosition(10)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        return MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBays(moveFeederBayInfos)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        // v3load
        Load v3load = getNetwork().getLoad("v3load");
        assertNotNull(v3load);
        String busbarId = ModificationUtils.getInstance().getBusOrBusbarSection(v3load.getTerminal());
        assertEquals("3A", busbarId);
        ConnectablePosition connectablePosition = v3load.getExtension(ConnectablePosition.class);
        assertNotNull(connectablePosition);
        ConnectablePosition.Feeder feeder = connectablePosition.getFeeder();
        assertNotNull(feeder);
        assertTrue(feeder.getName().isPresent());
        assertEquals("v3loadrename", feeder.getName().get());
        assertTrue(feeder.getOrder().isPresent());
        assertEquals(4, feeder.getOrder().get());
        assertEquals(ConnectablePosition.Direction.TOP, feeder.getDirection());

        // line2
        Line line2 = getNetwork().getLine("line2");
        assertNotNull(line2);
        String line2BusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(line2.getTerminal2());
        assertEquals("3C", line2BusbarId);
        ConnectablePosition lineConnectablePosition = line2.getExtension(ConnectablePosition.class);
        assertNotNull(lineConnectablePosition);
        ConnectablePosition.Feeder feeder2 = lineConnectablePosition.getFeeder2();
        assertNotNull(feeder2);
        assertTrue(feeder2.getName().isPresent());
        assertEquals("line2NameV3", feeder2.getName().get());
        assertTrue(feeder2.getOrder().isPresent());
        assertEquals(10, feeder2.getOrder().get());
        assertEquals(ConnectablePosition.Direction.BOTTOM, feeder2.getDirection());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // v3load
        Load v3load = getNetwork().getLoad("v3load");
        assertNotNull(v3load);
        String busbarId = ModificationUtils.getInstance().getBusOrBusbarSection(v3load.getTerminal());
        assertEquals("3A", busbarId);
        ConnectablePosition connectablePosition = v3load.getExtension(ConnectablePosition.class);
        assertNull(connectablePosition);

        // line2
        Line line2 = getNetwork().getLine("line2");
        assertNotNull(line2);
        String line2BusbarId = ModificationUtils.getInstance().getBusOrBusbarSection(line2.getTerminal2());
        assertEquals("3C", line2BusbarId);
        ConnectablePosition lineConnectablePosition = line2.getExtension(ConnectablePosition.class);
        assertNotNull(lineConnectablePosition);
        ConnectablePosition.Feeder feeder2 = lineConnectablePosition.getFeeder2();
        assertNotNull(feeder2);
        assertTrue(feeder2.getName().isPresent());
        assertEquals("cn2line2", feeder2.getName().get());
        assertTrue(feeder2.getOrder().isPresent());
        assertEquals(2, feeder2.getOrder().get());
        assertEquals(ConnectablePosition.Direction.TOP, feeder2.getDirection());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("v3", createdValues.get("voltageLevelId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v3", updatedValues.get("voltageLevelId"));
    }

    @Test
    void testGetTerminal() {
        Network network = getNetwork();
        // is not an injection or a branch
        MoveFeederBayInfos connectablePositionModification = MoveFeederBayInfos.builder()
            .equipmentId("trf6")
            .busbarSectionId("1A")
            .connectionSide("ONE")
            .connectionName("trf6Moved")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos = MoveVoltageLevelFeederBaysInfos.builder()
                .voltageLevelId("v2")
                .feederBays(List.of(connectablePositionModification))
                .build();
        MoveVoltageLevelFeederBays moveVoltageLevelFeederBays = (MoveVoltageLevelFeederBays) moveVoltageLevelFeederBaysInfos.toModification();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS", moveVoltageLevelFeederBays.getName());
        String message = assertThrows(RuntimeException.class, () -> moveVoltageLevelFeederBays.getTerminal(network, connectablePositionModification)).getMessage();
        assertEquals("MoveVoltageLevelFeederBays is not implemented for class com.powsybl.network.store.iidm.impl.ThreeWindingsTransformerImpl", message);
        // busbar not found on a branch
        MoveFeederBayInfos connectablePositionModification2 = MoveFeederBayInfos.builder()
            .equipmentId("line1")
            .busbarSectionId("random")
            .connectionSide("THREE")
            .connectionName("line1")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        message = assertThrows(RuntimeException.class, () -> moveVoltageLevelFeederBays.getTerminal(network, connectablePositionModification2)).getMessage();
        assertEquals("Invalid connection side: THREE for branch line1", message);
        // injection with no error
        MoveFeederBayInfos connectablePositionModification3 = MoveFeederBayInfos.builder()
            .equipmentId("v3Battery")
            .busbarSectionId("3B")
            .connectionSide(null)
            .connectionName("v3Battery")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        Terminal terminal = moveVoltageLevelFeederBays.getTerminal(network, connectablePositionModification3);
        assertEquals("v3", terminal.getVoltageLevel().getId());
        assertEquals("v3Battery", terminal.getConnectable().getId());
        // branch side 2 with no error
        MoveFeederBayInfos connectablePositionModification4 = MoveFeederBayInfos.builder()
            .equipmentId("line1")
            .busbarSectionId("1.B")
            .connectionSide("TWO")
            .connectionName("line1")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
        terminal = moveVoltageLevelFeederBays.getTerminal(network, connectablePositionModification4);
        assertEquals("v4", terminal.getVoltageLevel().getId());
        assertEquals("line1", terminal.getConnectable().getId());
        assertEquals(TwoSides.TWO, network.getLine("line1").getSide(terminal));
    }
}
