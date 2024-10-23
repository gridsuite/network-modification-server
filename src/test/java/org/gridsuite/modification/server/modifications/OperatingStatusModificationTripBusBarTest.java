/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.IN_OPERATION;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("IntegrationTest")
class OperatingStatusModificationTripBusBarTest extends AbstractNetworkModificationTest {
    private static final String TARGET_BUSBAR_ID = "1.A";
    private static final OperatingStatus.Status TARGET_BUSBAR_STATUS = FORCED_OUTAGE;
    private static final OperatingStatus.Status INITIAL_BUSBAR_STATUS = IN_OPERATION;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_BUSBAR_ID, INITIAL_BUSBAR_STATUS);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_BUSBAR_ID)
                .action(OperatingStatusModificationInfos.ActionType.TRIP).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_BUSBAR_ID)
                .action(OperatingStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_BUSBAR_ID, TARGET_BUSBAR_STATUS);
        assertTerminalsStatusAfterNetworkModification(false);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // back to init status
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_BUSBAR_ID, INITIAL_BUSBAR_STATUS);
        assertTerminalsStatusAfterNetworkModification(true);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("TRIP", createdValues.get("action"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("SWITCH_ON", updatedValues.get("action"));
    }

    private void assertTerminalsStatusAfterNetworkModification(boolean shouldBeConnected) {
        BusbarSection busbarSection = getNetwork().getBusbarSection(TARGET_BUSBAR_ID);
        assertEquals(busbarSection.getTerminal().isConnected(), shouldBeConnected);
    }
}
