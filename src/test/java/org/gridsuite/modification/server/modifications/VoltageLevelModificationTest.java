/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
public class VoltageLevelModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return VoltageLevelModificationInfos.builder()
                .equipmentId("v1")
                .equipmentName(new AttributeModification<>("test 1", OperationType.SET))
                .nominalVoltage(new AttributeModification<>(420D, OperationType.SET))
                .lowVoltageLimit(new AttributeModification<>(30D, OperationType.SET))
                .highVoltageLimit(new AttributeModification<>(50D, OperationType.SET))
                .ipMax(new AttributeModification<>(0.8, OperationType.SET))
                .ipMin(new AttributeModification<>(0.7, OperationType.SET))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationWithOnlyMetadata() {
        ModificationInfos builtModificationInfos = buildModification();
        builtModificationInfos.setStashed(false);
        builtModificationInfos.setMessageType("VOLTAGE_LEVEL_MODIFICATION");
        builtModificationInfos.setMessageValues("{\"equipmentId\":\"v1\"}");
        return builtModificationInfos;
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VoltageLevelModificationInfos.builder()
                .equipmentId("v1")
                .messageType("VOLTAGE_LEVEL_MODIFICATION")
                .messageValues("{\"equipmentId\":\"v1\"}")
                .stashed(false)
                .equipmentName(new AttributeModification<>("test 2", OperationType.SET))
                .nominalVoltage(new AttributeModification<>(450D, OperationType.SET))
                .lowVoltageLimit(new AttributeModification<>(40D, OperationType.SET))
                .highVoltageLimit(new AttributeModification<>(55D, OperationType.SET))
                .ipMax(new AttributeModification<>(0.9, OperationType.SET))
                .ipMin(new AttributeModification<>(0.5, OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit);
        assertNotNull(voltageLevel);
        assertEquals("test 1", voltageLevel.getNameOrId());
        assertEquals(420D, voltageLevel.getNominalV(), 0);
        assertEquals(30D, voltageLevel.getLowVoltageLimit(), 0);
        assertEquals(50D, voltageLevel.getHighVoltageLimit(), 0);
        assertEquals(0.8, identifiableShortCircuit.getIpMax(), 0);
        assertEquals(0.7, identifiableShortCircuit.getIpMin(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertNull(identifiableShortCircuit);
        assertNotNull(voltageLevel);
        assertEquals(380D, voltageLevel.getNominalV(), 0);
        assertEquals(Double.NaN, voltageLevel.getLowVoltageLimit(), 0);
        assertEquals(Double.NaN, voltageLevel.getHighVoltageLimit(), 0);
    }

    @Test
    public void testModifyShortCircuitExtension() throws Exception {
        VoltageLevelModificationInfos infos = (VoltageLevelModificationInfos) buildModification();
        applyModification(infos);

        VoltageLevelModificationInfos updatedInfos = VoltageLevelModificationInfos.builder()
                .equipmentId("v1")
                .ipMax(new AttributeModification<>(0.9, OperationType.SET))
                .build();
        applyModification(updatedInfos);
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        assertNotNull(voltageLevel);

        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(0.9, identifiableShortCircuit1.getIpMax(), 0);
        assertEquals(0.7, identifiableShortCircuit1.getIpMin(), 0);

        VoltageLevelModificationInfos updatedInfos2 = VoltageLevelModificationInfos.builder()
                .equipmentId("v1")
                .ipMin(new AttributeModification<>(0.2, OperationType.SET))
                .build();
        applyModification(updatedInfos2);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit2 = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        assertEquals(0.9, identifiableShortCircuit2.getIpMax(), 0);
        assertEquals(0.2, identifiableShortCircuit2.getIpMin(), 0);
    }

    private void applyModification(VoltageLevelModificationInfos infos) throws Exception {
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(mapper.writeValueAsString(infos))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }
}
