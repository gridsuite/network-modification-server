/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.LoadingLimits.TemporaryLimit;
import com.powsybl.iidm.network.Network;
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

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.utils.assertions.Assertions.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@Tag("IntegrationTest")
public class LineModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(null)
                                .name("name31")
                                .value(null)
                                .modificationType(TemporaryLimitModificationType.ADDED)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(32)
                                .name("name32")
                                .value(42.0)
                                .modificationType(TemporaryLimitModificationType.ADDED)
                                .build()))
                        .build())
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified1", OperationType.SET))
                .x(new AttributeModification<>(1.1, OperationType.SET))
                .r(new AttributeModification<>(2.1, OperationType.SET))
                .g1(new AttributeModification<>(11.1, OperationType.SET))
                .b1(new AttributeModification<>(12.1, OperationType.SET))
                .g2(new AttributeModification<>(13.1, OperationType.SET))
                .b2(new AttributeModification<>(14.1, OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(21.1)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(33)
                                .name("name33")
                                .value(41.1)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.1)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(35)
                                .name("name35")
                                .value(42.1)
                                .build()))
                        .build())
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Line modifiedLine = getNetwork().getLine("line1");
        assertEquals("LineModified", modifiedLine.getNameOrId());
        assertEquals(1.0, modifiedLine.getR());
        assertEquals(1.0, modifiedLine.getX());
        assertEquals(1.0, modifiedLine.getG1());
        assertEquals(1.0, modifiedLine.getB1());
        assertEquals(2.0, modifiedLine.getG2());
        assertEquals(2.0, modifiedLine.getB2());
        assertEquals(Double.NaN, modifiedLine.getNullableCurrentLimits1().getPermanentLimit());
        TemporaryLimit temporaryLimit = modifiedLine.getNullableCurrentLimits1().getTemporaryLimit(Integer.MAX_VALUE);
        assertEquals(Integer.MAX_VALUE, temporaryLimit.getAcceptableDuration());
        assertEquals("name31", temporaryLimit.getName());
        assertEquals(Double.MAX_VALUE, temporaryLimit.getValue());
        assertEquals(22.0, modifiedLine.getNullableCurrentLimits2().getPermanentLimit());
        temporaryLimit = modifiedLine.getNullableCurrentLimits2().getTemporaryLimit(32);
        assertEquals(32, temporaryLimit.getAcceptableDuration());
        assertEquals("name32", temporaryLimit.getName());
        assertEquals(42.0, temporaryLimit.getValue());

    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        Line line = getNetwork().getLine("line1");
        assertEquals("line1", line.getNameOrId());
        assertEquals(1.0, line.getR());
        assertEquals(1.0, line.getX());
        assertEquals(1.0, line.getG1());
        assertEquals(1.0, line.getB1());
        assertEquals(2.0, line.getG2());
        assertEquals(2.0, line.getB2());
        assertNull(line.getNullableCurrentLimits1());
        assertNull(line.getNullableCurrentLimits2());
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();
        lineModificationInfos.setEquipmentId("lineNotFound");
        String lineModificationInfosJson = mapper.writeValueAsString(lineModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri())
                .content(lineModificationInfosJson)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "Line lineNotFound does not exist in network").getMessage(),
                lineModificationInfos.getErrorType().name(), reportService);
    }

    @Test
    public void testPermanentLimitUnchanged() throws Exception {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();

        lineModificationInfos.getCurrentLimits1().setPermanentLimit(null);
        lineModificationInfos.getCurrentLimits2().setPermanentLimit(null);
        String modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);
    }

    @Test
    public void testCharacteristicsModification() throws Exception {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();

        // Modify Series Reactance
        lineModificationInfos.setX(new AttributeModification<>(1.0, OperationType.SET));

        String modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Series Resistance
        lineModificationInfos.setX(null);
        lineModificationInfos.setR(new AttributeModification<>(2.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Conductance1
        lineModificationInfos.setR(null);
        lineModificationInfos.setG1(new AttributeModification<>(11.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Susceptance1
        lineModificationInfos.setG1(null);
        lineModificationInfos.setB1(new AttributeModification<>(12.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Conductance2
        lineModificationInfos.setB1(null);
        lineModificationInfos.setG2(new AttributeModification<>(13.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Susceptance2
        lineModificationInfos.setG2(null);
        lineModificationInfos.setB2(new AttributeModification<>(14.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // no modification
        lineModificationInfos.setB2(null);
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(6);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify all
        lineModificationInfos.setX(new AttributeModification<>(1.0, OperationType.SET));
        lineModificationInfos.setX(new AttributeModification<>(2.0, OperationType.SET));
        lineModificationInfos.setG1(new AttributeModification<>(11.0, OperationType.SET));
        lineModificationInfos.setB1(new AttributeModification<>(12.0, OperationType.SET));
        lineModificationInfos.setG2(new AttributeModification<>(13.0, OperationType.SET));
        lineModificationInfos.setB2(new AttributeModification<>(14.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true)
                        .get(7);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);
    }

    @Test
    public void testTemporaryLimitsModification() throws Exception {
        Line line = getNetwork().getLine("line1");
        line.newCurrentLimits1()
                .setPermanentLimit(10.0)
                .beginTemporaryLimit()
                .setName("name31")
                .setAcceptableDuration(Integer.MAX_VALUE)
                .setValue(Double.MAX_VALUE)
                .endTemporaryLimit()
                .add();
        line.newCurrentLimits2()
                .setPermanentLimit(11.0)
                .beginTemporaryLimit()
                .setName("name32")
                .setAcceptableDuration(32)
                .setValue(15.0)
                .endTemporaryLimit()
                .beginTemporaryLimit()
                .setName("name33")
                .setAcceptableDuration(33)
                .setValue(15.0)
                .endTemporaryLimit()
                .add();
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(null)
                                .name("name31")
                                .value(22.0)
                                .modificationType(TemporaryLimitModificationType.MODIFIED)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(33)
                                .name("name33")
                                .value(15.0)
                                .modificationType(TemporaryLimitModificationType.DELETED)
                                .build()))
                        .build())
                .build();
        String modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify name and no modification on temporary limits
        line.setName(null);
        LineModificationInfos lineModificationInfos1 = LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line1")
                .equipmentName(new AttributeModification<>("ModifiedName", OperationType.SET))
                .build();
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos1);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos1);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line1", updatedValues.get("equipmentId"));
    }

    @Test
    public void testDisconnection() throws Exception {
        changeLineConnectionState(getNetwork().getLine("line1"), false);
    }

    @Test
    public void testConnection() throws Exception {
        changeLineConnectionState(getNetwork().getLine("line1"), true);
    }

    private void changeLineConnectionState(Line existingEquipment, boolean expectedState) throws Exception {
        LineModificationInfos modificationInfos = (LineModificationInfos) buildModification();
        modificationInfos.setConnected1(new AttributeModification<>(expectedState, OperationType.SET));
        modificationInfos.setConnected2(new AttributeModification<>(expectedState, OperationType.SET));

        if (expectedState) {
            if (existingEquipment.getTerminal1().isConnected()) {
                existingEquipment.getTerminal1().disconnect();
            }
            if (existingEquipment.getTerminal2().isConnected()) {
                existingEquipment.getTerminal2().disconnect();
            }
        } else {
            if (!existingEquipment.getTerminal1().isConnected()) {
                existingEquipment.getTerminal1().connect();
            }
            if (!existingEquipment.getTerminal2().isConnected()) {
                existingEquipment.getTerminal2().connect();
            }
        }
        assertThat(existingEquipment.getTerminal1().isConnected()).isNotEqualTo(expectedState);
        assertThat(existingEquipment.getTerminal2().isConnected()).isNotEqualTo(expectedState);

        String modificationInfosJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // connection state has changed as expected
        assertThat(existingEquipment.getTerminal1().isConnected()).isEqualTo(expectedState);
        assertThat(existingEquipment.getTerminal2().isConnected()).isEqualTo(expectedState);

        // try to modify again => no change on connection state
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertThat(existingEquipment.getTerminal1().isConnected()).isEqualTo(expectedState);
        assertThat(existingEquipment.getTerminal2().isConnected()).isEqualTo(expectedState);
    }
}
