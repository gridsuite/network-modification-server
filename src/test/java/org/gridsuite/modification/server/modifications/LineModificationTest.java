/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.LoadingLimits.TemporaryLimit;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;
import org.apache.commons.collections4.CollectionUtils;
import org.assertj.core.api.Assertions;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@Tag("IntegrationTest")
class LineModificationTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double MEASUREMENT_P_VALUE = 10.0;
    private static final Double MEASUREMENT_Q_VALUE = -10.0;
    private static final Boolean MEASUREMENT_P_VALID = true;
    private static final Boolean MEASUREMENT_Q_VALID = false;

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
                .voltageLevelId1(new AttributeModification<>("v1", OperationType.SET))
                .voltageLevelId2(new AttributeModification<>("v4", OperationType.SET))
                .busOrBusbarSectionId1(new AttributeModification<>("1B", OperationType.SET))
                .busOrBusbarSectionId2(new AttributeModification<>("2B", OperationType.SET))
                .connectionName1(new AttributeModification<>("cn1Line1", OperationType.SET))
                .connectionName2(new AttributeModification<>("cn2Line1", OperationType.SET))
                .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionDirection2(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
                .connectionPosition2(new AttributeModification<>(1, OperationType.SET))
                .p1MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p1MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .p2MeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
                .p2MeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
                .q1MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q1MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
                .q2MeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
                .q2MeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(12.0)
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
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
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
        assertEquals(12.0, modifiedLine.getNullableCurrentLimits1().getPermanentLimit());
        TemporaryLimit temporaryLimit = modifiedLine.getNullableCurrentLimits1().getTemporaryLimit(Integer.MAX_VALUE);
        assertEquals(Integer.MAX_VALUE, temporaryLimit.getAcceptableDuration());
        assertEquals("name31", temporaryLimit.getName());
        assertEquals(Double.MAX_VALUE, temporaryLimit.getValue());
        assertEquals(22.0, modifiedLine.getNullableCurrentLimits2().getPermanentLimit());
        temporaryLimit = modifiedLine.getNullableCurrentLimits2().getTemporaryLimit(32);
        assertEquals(32, temporaryLimit.getAcceptableDuration());
        assertEquals("name32", temporaryLimit.getName());
        assertEquals(42.0, temporaryLimit.getValue());
        assertEquals(PROPERTY_VALUE, modifiedLine.getProperty(PROPERTY_NAME));
        assertMeasurements(modifiedLine); // measurements extension are added
    }

    private void assertMeasurements(Line line) {
        Measurements<?> measurements = (Measurements<?>) line.getExtension(Measurements.class);
        assertNotNull(measurements);
        Collection<Measurement> activePowerMeasurements = measurements.getMeasurements(Measurement.Type.ACTIVE_POWER).stream().toList();
        assertNotNull(activePowerMeasurements);
        assertFalse(CollectionUtils.isEmpty(activePowerMeasurements));
        Assertions.assertThat(activePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_P_VALUE && m.isValid() == MEASUREMENT_P_VALID);
        Collection<Measurement> reactivePowerMeasurements = measurements.getMeasurements(Measurement.Type.REACTIVE_POWER).stream().toList();
        assertFalse(CollectionUtils.isEmpty(reactivePowerMeasurements));
        Assertions.assertThat(reactivePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_Q_VALUE && m.isValid() == MEASUREMENT_Q_VALID);
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
        assertNull(line.getProperty(PROPERTY_NAME));
        // no more measurements extension
        Measurements<?> measurements = (Measurements<?>) line.getExtension(Measurements.class);
        assertTrue(measurements == null || CollectionUtils.isEmpty(measurements.getMeasurements()));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();
        lineModificationInfos.setEquipmentId("lineNotFound");
        String lineModificationInfosJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri())
                .content(lineModificationInfosJson)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "Line 'lineNotFound' : does not exist in network").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testPermanentLimitUnchanged() throws Exception {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();

        lineModificationInfos.getCurrentLimits1().setPermanentLimit(null);
        lineModificationInfos.getCurrentLimits2().setPermanentLimit(null);
        String modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);
    }

    @Test
    void testCharacteristicsModification() throws Exception {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();

        // Modify Series Reactance
        lineModificationInfos.setX(new AttributeModification<>(1.0, OperationType.SET));

        String modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Series Resistance
        lineModificationInfos.setX(null);
        lineModificationInfos.setR(new AttributeModification<>(2.0, OperationType.SET));
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Conductance1
        lineModificationInfos.setR(null);
        lineModificationInfos.setG1(new AttributeModification<>(11.0, OperationType.SET));
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Susceptance1
        lineModificationInfos.setG1(null);
        lineModificationInfos.setB1(new AttributeModification<>(12.0, OperationType.SET));
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Conductance2
        lineModificationInfos.setB1(null);
        lineModificationInfos.setG2(new AttributeModification<>(13.0, OperationType.SET));
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // Modify Shunt Susceptance2
        lineModificationInfos.setG2(null);
        lineModificationInfos.setB2(new AttributeModification<>(14.0, OperationType.SET));
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);

        // no modification
        lineModificationInfos.setB2(null);
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

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
        modificationToCreateJson = getJsonBody(lineModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true)
                        .get(7);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos);
    }

    @Test
    void testTemporaryLimitsModification() throws Exception {
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
                        .permanentLimit(12.0)
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
        String modificationToCreateJson = getJsonBody(lineModificationInfos, null);

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
        modificationToCreateJson = getJsonBody(lineModificationInfos1, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(lineModificationInfos1);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line1", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line1", updatedValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        changeLineConnectionState(getNetwork().getLine("line1"), false);
    }

    @Test
    void testConnection() throws Exception {
        changeLineConnectionState(getNetwork().getLine("line1"), true);
    }

    private void changeLineConnectionState(Line existingEquipment, boolean expectedState) throws Exception {
        LineModificationInfos modificationInfos = (LineModificationInfos) buildModification();
        modificationInfos.setTerminal1Connected(new AttributeModification<>(expectedState, OperationType.SET));
        modificationInfos.setTerminal2Connected(new AttributeModification<>(expectedState, OperationType.SET));

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

        String modificationInfosJson = getJsonBody(modificationInfos, null);
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

    @Test
    void changeLineConnectablePosition() throws Exception {
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line3")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .voltageLevelId1(new AttributeModification<>("v1", OperationType.SET))
                .voltageLevelId2(new AttributeModification<>("v3", OperationType.SET))
                .busOrBusbarSectionId1(new AttributeModification<>("1B", OperationType.SET))
                .busOrBusbarSectionId2(new AttributeModification<>("2B", OperationType.SET))
                .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
                .connectionPosition2(new AttributeModification<>(1, OperationType.SET))
                .build();
        String modificationInfosJson = getJsonBody(lineModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals(1, createdModification.getConnectionPosition1().getValue());
        assertEquals(1, createdModification.getConnectionPosition2().getValue());
    }

    @Test
    void changeLineConnectablePositionWithoutBusBarSection() throws Exception {
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line3")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .connectionName1(new AttributeModification<>("line3", OperationType.SET))
                .connectionName2(new AttributeModification<>("line3", OperationType.SET))
                .build();
        String modificationInfosJson = getJsonBody(lineModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals("line3", createdModification.getConnectionName1().getValue());
        assertEquals("line3", createdModification.getConnectionName2().getValue());

    }

    @Test
    void changeLineWithConnectablePositionOneSide() throws Exception {
        var l = getNetwork().newLine()
                .setId("line10")
                .setName("line10")
                .setR(1.0)
                .setX(1.0)
                .setB1(1.0)
                .setB2(1.0)
                .setVoltageLevel1("v3")
                .setVoltageLevel2("v4")
                .setNode1(7)
                .setNode2(3)
                .add();
        l.newExtension(ConnectablePositionAdder.class)
                .newFeeder1()
                .withName("line10")
                .withOrder(0)
                .withDirection(ConnectablePosition.Direction.UNDEFINED).add()
                .add();
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
                .stashed(false)
                .equipmentId("line10")
                .connectionName1(new AttributeModification<>("cnLine10", OperationType.SET))
                .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionPosition1(new AttributeModification<>(2, OperationType.SET))
                .build();

        String modificationInfosJson = getJsonBody(lineModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals("cnLine10", createdModification.getConnectionName1().getValue());
        assertEquals(2, createdModification.getConnectionPosition1().getValue());
        assertEquals(ConnectablePosition.Direction.TOP, createdModification.getConnectionDirection1().getValue());
        assertNull(createdModification.getConnectionName2());
        assertNull(createdModification.getConnectionDirection2());
        assertNull(createdModification.getConnectionPosition2());
    }

    private void assertConnectablePositionImpacts(List<AbstractBaseImpact> impacts) {
        assertEquals(2, impacts.size());
        assertTrue(impacts.get(0).isSimple());
        SimpleElementImpact simpleImpact = (SimpleElementImpact) impacts.get(0);
        assertEquals(IdentifiableType.SUBSTATION, simpleImpact.getElementType());
        assertEquals("s1", simpleImpact.getElementId());
        assertTrue(impacts.get(1).isSimple());
        simpleImpact = (SimpleElementImpact) impacts.get(1);
        assertEquals(IdentifiableType.SUBSTATION, simpleImpact.getElementType());
        assertEquals("s2", simpleImpact.getElementId());
    }

    @Test
    void addConnectablePositionExtensionToLine() throws Exception {
        // creating new extension ConnectablePosition on line3 (which has previously no ConnectablePosition extension),
        // by only setting this extension fields in the modification applied
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder()
            .stashed(false)
            .equipmentId("line3")
            .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
            .connectionName1(new AttributeModification<>("feeder1", OperationType.SET))
            .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.BOTTOM, OperationType.SET))
            .build();
        String modificationInfosJson = getJsonBody(lineModificationInfos, null);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationsResult> networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationsResult.isPresent());

        // the extension creation notification leads to creating first a simple impact for the line line3, which then leads to the creation
        // of 2 simple impacts on both line substations (see reduceNetworkImpacts method called in NetworkStoreListener)
        assertConnectablePositionImpacts(getNetworkImpacts(networkModificationsResult.get()));

        // update position field in this existing extension
        lineModificationInfos = LineModificationInfos.builder()
            .stashed(false)
            .equipmentId("line3")
            .connectionPosition1(new AttributeModification<>(2, OperationType.SET))
            .build();
        modificationInfosJson = getJsonBody(lineModificationInfos, null);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationsResult.isPresent());

        // the extension update notification leads to creating first a simple impact for the line line3, which then leads to the creation
        // of 2 simple impacts on both line substations (see reduceNetworkImpacts method called in NetworkStoreListener)
        assertConnectablePositionImpacts(getNetworkImpacts(networkModificationsResult.get()));
    }
}
