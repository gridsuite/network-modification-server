/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
public class VoltageLevelModificationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1")
                .equipmentName(new AttributeModification<>("test 1", OperationType.SET))
                .nominalV(new AttributeModification<>(420D, OperationType.SET))
                .lowVoltageLimit(new AttributeModification<>(30D, OperationType.SET))
                .highVoltageLimit(new AttributeModification<>(50D, OperationType.SET))
                .ipMax(new AttributeModification<>(0.8, OperationType.SET))
                .ipMin(new AttributeModification<>(0.7, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1Edited")
                .equipmentName(new AttributeModification<>("test 2", OperationType.SET))
                .nominalV(new AttributeModification<>(450D, OperationType.SET))
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
        assertEquals(PROPERTY_VALUE, getNetwork().getVoltageLevel("v1").getProperty(PROPERTY_NAME));
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
                .stashed(false)
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

    private void testIpMinIpMaxNotChanged(Double ipMin, Double ipMax, String reportError) throws Exception {
        final String vlWithBothIcc = "v3";
        final double beforeUpdateIpMin = 15.0; // cf NetworkCreation.java
        final double beforeUpdateIpMax = 25.0;

        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId(vlWithBothIcc)
                .build();
        if (ipMin != null) {
            vli.setIpMin(new AttributeModification<>(ipMin, OperationType.SET));
        }
        if (ipMax != null) {
            vli.setIpMax(new AttributeModification<>(ipMax, OperationType.SET));
        }
        applyModification(vli);

        // check the update has not been made
        VoltageLevel voltageLevelUpdated = getNetwork().getVoltageLevel(vlWithBothIcc);
        assertNotNull(voltageLevelUpdated);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevelUpdated.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(beforeUpdateIpMin, identifiableShortCircuit1.getIpMin(), 0);
        assertEquals(beforeUpdateIpMax, identifiableShortCircuit1.getIpMax(), 0);
        assertLogMessage(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, reportError).getMessage(), vli.getErrorType().name(), reportService);
    }

    @Test
    public void testIpMinGreaterThanIpMax() throws Exception {
        // check only modification inputs
        testIpMinIpMaxNotChanged(30.0, 29.0, "IpMin cannot be greater than IpMax");
    }

    @Test
    public void testIpMinNegative() throws Exception {
        // check only modification inputs
        testIpMinIpMaxNotChanged(-30.0, 0.0, "IpMin must be positive");
    }

    @Test
    public void testIpMaxNegative() throws Exception {
        // check only modification inputs
        testIpMinIpMaxNotChanged(0.0, -12.0, "IpMax must be positive");
    }

    @Test
    public void testIpMinGreaterThanEquipmentIpMax() throws Exception {
        // check ipMin modification input against equipement ipMax real value (25.0)
        testIpMinIpMaxNotChanged(30.0, null, "IpMin cannot be greater than IpMax");
    }

    @Test
    public void testEquipmentIpMinGreaterThanIpMax() throws Exception {
        // check ipMax modification input against equipement ipMin real value (15.0)
        testIpMinIpMaxNotChanged(null, 14.9, "IpMin cannot be greater than IpMax");
    }

    @Test
    public void testIpMinEqualsIpMax() throws Exception {
        final String vlWithBothIcc = "v3";
        final double iccValue = 29.0;
        VoltageLevelModificationInfos vli = (VoltageLevelModificationInfos) buildModification();
        vli.setIpMin(new AttributeModification<>(iccValue, OperationType.SET));
        vli.setIpMax(new AttributeModification<>(iccValue, OperationType.SET));
        vli.setEquipmentId(vlWithBothIcc);
        applyModification(vli);

        // check the update has been made
        VoltageLevel voltageLevelUpdated = getNetwork().getVoltageLevel(vlWithBothIcc);
        assertNotNull(voltageLevelUpdated);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevelUpdated.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(iccValue, identifiableShortCircuit1.getIpMin(), 0);
        assertEquals(iccValue, identifiableShortCircuit1.getIpMax(), 0);
    }

    @Test
    public void testSetIpMinOnEquipmentWithoutExtension() throws Exception {
        final String vlWithNoIcc = "v2";
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId(vlWithNoIcc)
                .ipMin(new AttributeModification<>(10.0, OperationType.SET))
                .build();
        applyModification(vli);
        // check the update has not been made
        VoltageLevel voltageLevelUpdated = getNetwork().getVoltageLevel(vlWithNoIcc);
        assertNotNull(voltageLevelUpdated);
        assertNull(voltageLevelUpdated.getExtension(IdentifiableShortCircuit.class));
        assertLogMessage(new NetworkModificationException(MODIFY_VOLTAGE_LEVEL_ERROR, "IpMax is required").getMessage(), vli.getErrorType().name(), reportService);
    }

    @Test
    public void testSetIpMaxOnEquipmentWitOnlyIpMaxExtension() throws Exception {
        final String vlName = "v2"; // has no ICC
        getNetwork().getVoltageLevel(vlName)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMax(30.0).add();

        final double targetIpMax = 29.0;
        VoltageLevelModificationInfos vli = VoltageLevelModificationInfos.builder()
                .stashed(false)
                .equipmentId(vlName)
                .ipMax(new AttributeModification<>(targetIpMax, OperationType.SET))
                .build();
        applyModification(vli);
        // check the update has been made
        VoltageLevel voltageLevelUpdated = getNetwork().getVoltageLevel(vlName);
        assertNotNull(voltageLevelUpdated);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit1 = voltageLevelUpdated.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit1);
        assertEquals(0, identifiableShortCircuit1.getIpMin(), 0);
        assertEquals(targetIpMax, identifiableShortCircuit1.getIpMax(), 0);
    }

    private void applyModification(VoltageLevelModificationInfos infos) throws Exception {
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(mapper.writeValueAsString(infos))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("VOLTAGE_LEVEL_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("VOLTAGE_LEVEL_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1Edited", updatedValues.get("equipmentId"));
    }
}
