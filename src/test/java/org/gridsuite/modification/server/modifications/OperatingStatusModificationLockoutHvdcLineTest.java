/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.PLANNED_OUTAGE;
import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("IntegrationTest")
public class OperatingStatusModificationLockoutHvdcLineTest extends AbstractNetworkModificationTest {

    private static final String TARGET_HVDC_LINE_ID = "hvdcLine";

    private static final OperatingStatus.Status TARGET_HVDC_LINE_STATUS = PLANNED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force operating status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_HVDC_LINE_ID, TARGET_HVDC_LINE_STATUS);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_HVDC_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.LOCKOUT).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId("hvdcLineEdited")
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.LOCKOUT).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_HVDC_LINE_ID, TARGET_HVDC_LINE_STATUS);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // go back to init status
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_HVDC_LINE_ID, TARGET_HVDC_LINE_STATUS);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("LOCKOUT", createdValues.get("action"));
        assertEquals("hvdcLine", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", updatedValues.get("energizedVoltageLevelId"));
        assertEquals("LOCKOUT", updatedValues.get("action"));
        assertEquals("hvdcLineEdited", updatedValues.get("equipmentId"));
    }
}
