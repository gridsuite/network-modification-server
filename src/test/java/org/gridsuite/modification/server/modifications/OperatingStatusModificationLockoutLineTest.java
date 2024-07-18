/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.iidm.network.util.SwitchPredicates;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.PLANNED_OUTAGE;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class OperatingStatusModificationLockoutLineTest extends AbstractNetworkModificationTest {

    private static final String TARGET_LINE_ID = "line2";
    private static final String UPDATE_BRANCH_ID = "line1";
    private static final OperatingStatus.Status TARGET_BRANCH_STATUS = PLANNED_OUTAGE;
    private static final OperatingStatus.Status OTHER_BRANCH_STATUS = FORCED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.create(networkUuid, true);
        // force a branch status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_LINE_ID, OTHER_BRANCH_STATUS);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.LOCKOUT).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(UPDATE_BRANCH_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_LINE_ID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // go back to init status
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_LINE_ID, OTHER_BRANCH_STATUS);
    }

    @Test
    public void testCreate() throws Exception {
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        //test lockout line modification with non-fictional switch
        getNetwork().getSubstation("s1").newVoltageLevel().setId("v1f").setFictitious(true)
                .setTopologyKind(TopologyKind.NODE_BREAKER).setNominalV(380.).add();
        getNetwork().getSubstation("s2").newVoltageLevel().setId("v3f").setFictitious(true)
                .setTopologyKind(TopologyKind.NODE_BREAKER).setNominalV(380.).add();

        Line line = getNetwork().newLine()
                .setId("nonFictionalDisconnect")
                .setVoltageLevel1("v1f")
                .setVoltageLevel2("v3f")
                .setNode1(0)
                .setNode2(0)
                .setX(12)
                .setR(7)
                .add();
        assertNotNull(line);

        line.getTerminal("v1f").disconnect(SwitchPredicates.IS_NONFICTIONAL);
        line.getTerminal("v3f").disconnect(SwitchPredicates.IS_NONFICTIONAL);
        assertNotNull(line);
        modificationInfos.setEquipmentId("nonFictionalDisconnect");
        modificationInfos.setAction(OperatingStatusModificationInfos.ActionType.LOCKOUT);
        String modificationJson = mapper.writeValueAsString(modificationInfos);
        assertNull(getNetwork().getLine("nonFictionalDisconnect").getExtension(OperatingStatus.class));

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(getNetwork().getLine("nonFictionalDisconnect").getExtension(OperatingStatus.class));

    }

    @Test
    public void testCreateWithErrors() throws Exception {
        // line not existing
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId("notFound");
        String modificationJson = mapper.writeValueAsString(modificationInfos);

        // modification action not existing
        modificationJson = modificationJson.replace("LOCKOUT", "INVALID_ACTION"); // note: should never happen in real
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(
                        status().is4xxClientError());

        Line line = getNetwork().newLine()
                .setId("cantdisconnect")
                .setVoltageLevel1("v1")
                .setVoltageLevel2("v3")
                .setNode1(0)
                .setNode2(0)
                .setX(12)
                .setR(7)
                .add();
        assertNotNull(line);
        modificationInfos.setEquipmentId("cantdisconnect");
        modificationInfos.setAction(OperatingStatusModificationInfos.ActionType.LOCKOUT);
        modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNull(getNetwork().getLine("cantdisconnect").getExtension(OperatingStatus.class));
        assertLogMessage(new NetworkModificationException(OPERATING_STATUS_MODIFICATION_ERROR, "Unable to disconnect all equipment ends").getMessage(),
                modificationInfos.getErrorType().name(), reportService);

    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("LOCKOUT", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", updatedValues.get("energizedVoltageLevelId"));
        assertEquals("SWITCH_ON", updatedValues.get("action"));
        assertEquals("line1", updatedValues.get("equipmentId"));
    }
}
