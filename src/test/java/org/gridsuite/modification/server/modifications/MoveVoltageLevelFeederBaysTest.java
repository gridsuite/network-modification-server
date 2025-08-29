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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ConnectablePositionModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.MoveVoltageLevelFeederBaysInfos;
import org.gridsuite.modification.modifications.ConnectablePositionModification;
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
public class MoveVoltageLevelFeederBaysTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        createBusBarSection(network.getVoltageLevel("v3"), "3B", "3B", 20);
        createBusBarSection(network.getVoltageLevel("v3"), "3C", "3C", 30);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ConnectablePositionModificationInfos> connectablePositionModificationInfos = new ArrayList<>();
        connectablePositionModificationInfos.add(ConnectablePositionModificationInfos.builder()
            .connectableId("v3load")
            .busBarSectionId("3A")
            .targetBusBarSectionId("3B")
            .connectionName("v3loadrename")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        connectablePositionModificationInfos.add(ConnectablePositionModificationInfos.builder()
            .connectableId("line2")
            .busBarSectionId("3C")
            .targetBusBarSectionId("3B")
            .connectionName("line2NameV3")
            .connectionPosition(10)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build());
        return MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBaysAttributeList(connectablePositionModificationInfos)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ConnectablePositionModificationInfos> connectablePositionModificationInfos = new ArrayList<>();
        connectablePositionModificationInfos.add(ConnectablePositionModificationInfos.builder()
            .connectableId("v3load")
            .busBarSectionId("3A")
            .targetBusBarSectionId("3B")
            .connectionName("v3loadmodified")
            .connectionPosition(6)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .build());
        connectablePositionModificationInfos.add(ConnectablePositionModificationInfos.builder()
            .connectableId("line2")
            .busBarSectionId("3B")
            .targetBusBarSectionId("3C")
            .connectionName("line2modifiedbis")
            .connectionPosition(10)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        return MoveVoltageLevelFeederBaysInfos.builder()
            .voltageLevelId("v3")
            .feederBaysAttributeList(connectablePositionModificationInfos)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        // v3load
        Load v3load = getNetwork().getLoad("v3load");
        assertNotNull(v3load);
        String busbarId = ModificationUtils.getInstance().getBusOrBusbarSection(v3load.getTerminal());
        assertEquals("3B", busbarId);
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
        assertEquals("3B", line2BusbarId);
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
        ConnectablePositionModification connectablePositionModification = new ConnectablePositionModification(ConnectablePositionModificationInfos.builder()
            .connectableId("trf6")
            .busBarSectionId("1A")
            .targetBusBarSectionId("1B")
            .connectionName("trf6Moved")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        assertEquals("ConnectablePositionModification", connectablePositionModification.getName());
        String message = assertThrows(NetworkModificationException.class, () -> connectablePositionModification.getTerminal(network)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : ConnectablePositionModification is not implemented for class com.powsybl.network.store.iidm.impl.ThreeWindingsTransformerImpl", message);
        // busbar not found on a branch
        ConnectablePositionModification connectablePositionModification2 = new ConnectablePositionModification(ConnectablePositionModificationInfos.builder()
            .connectableId("line1")
            .busBarSectionId("random")
            .targetBusBarSectionId("3B")
            .connectionName("line1")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        message = assertThrows(NetworkModificationException.class, () -> connectablePositionModification2.getTerminal(network)).getMessage();
        assertEquals("MOVE_VOLTAGE_LEVEL_FEEDER_BAYS_ERROR : the busbar id random does not correspond to any of the busbar of line1", message);
        // injection with no error
        ConnectablePositionModification connectablePositionModification3 = new ConnectablePositionModification(ConnectablePositionModificationInfos.builder()
            .connectableId("v3Battery")
            .busBarSectionId("3A")
            .targetBusBarSectionId("3B")
            .connectionName("v3Battery")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        Terminal terminal = connectablePositionModification3.getTerminal(network);
        assertEquals("v3", terminal.getVoltageLevel().getId());
        assertEquals("v3Battery", terminal.getConnectable().getId());
        // branch side 2 with no error
        ConnectablePositionModification connectablePositionModification4 = new ConnectablePositionModification(ConnectablePositionModificationInfos.builder()
            .connectableId("line1")
            .busBarSectionId("1.A")
            .targetBusBarSectionId("1.B")
            .connectionName("line1")
            .connectionPosition(4)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build());
        terminal = connectablePositionModification4.getTerminal(network);
        assertEquals("v4", terminal.getVoltageLevel().getId());
        assertEquals("line1", terminal.getConnectable().getId());
        assertEquals(TwoSides.TWO, network.getLine("line1").getSide(terminal));
    }
}
