/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class LineCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    @Test
    public void testCreateWithBadVariant() throws Exception {
        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        LineCreationInfos modificationToCreate = (LineCreationInfos) buildModification();
        modificationToCreate.setEquipmentId("idLine2");
        modificationToCreate.setEquipmentName("nameLine2");
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(networkModificationResult);
        assertTrue(networkModificationResult.isEmpty());  // no modifications returned
        assertNull(getNetwork().getLine("idLine2"));  // line was not created
        testNetworkModificationsCount(getGroupId(), 1);  // new modification stored in the database
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        LineCreationInfos lineCreationInfos = (LineCreationInfos) buildModification();
        lineCreationInfos.setEquipmentId("");
        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", lineCreationInfos.getErrorType().name(), reportService);

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage(),
                lineCreationInfos.getErrorType().name(), reportService);

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage(),
                lineCreationInfos.getErrorType().name(), reportService);

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setSeriesResistance(Double.NaN);
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("AC Line 'idLine4': r is invalid", lineCreationInfos.getErrorType().name(), reportService);

        lineCreationInfos.setSeriesResistance(100.0);
        lineCreationInfos.setSeriesReactance(Double.NaN);
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("AC Line 'idLine4': x is invalid", lineCreationInfos.getErrorType().name(), reportService);

        // try to create an existing line
        lineCreationInfos.setEquipmentId("line2");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "line2").getMessage(),
                lineCreationInfos.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateLineWithOnlyPermanentCurrentLimits() throws Exception {
        LineCreationInfos lineCreation = LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .seriesResistance(110.0)
                .seriesReactance(110.0)
                .shuntConductance1(15.0)
                .shuntSusceptance1(15.0)
                .shuntConductance2(25.0)
                .shuntSusceptance2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(6.).build())
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();

        String lineCreationJson = mapper.writeValueAsString(lineCreation);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals(5., createdModification.getCurrentLimits1().getPermanentLimit(), 0.);
        assertEquals(6., createdModification.getCurrentLimits2().getPermanentLimit(), 0.);
        assertTrue(createdModification.getCurrentLimits1().getTemporaryLimits().isEmpty());
        assertTrue(createdModification.getCurrentLimits2().getTemporaryLimits().isEmpty());

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    public void testCreateLineWithOnlyTemporaryCurrentLimits() throws Exception {
        LineCreationInfos lineCreation = LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .seriesResistance(110.0)
                .seriesReactance(110.0)
                .shuntConductance1(15.0)
                .shuntSusceptance1(15.0)
                .shuntConductance2(25.0)
                .shuntSusceptance2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .currentLimits1(CurrentLimitsInfos.builder()
                    .temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").value(200.0).acceptableDuration(600).build()))
                    .build())
                .currentLimits2(CurrentLimitsInfos.builder()
                    .temporaryLimits(List.of(
                        CurrentTemporaryLimitCreationInfos.builder().name("IT10").value(200.0).acceptableDuration(600).build(),
                        CurrentTemporaryLimitCreationInfos.builder().name("IT20").value(100.0).acceptableDuration(1200).build()))
                    .build())
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();

        String lineCreationJson = mapper.writeValueAsString(lineCreation);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertNull(createdModification.getCurrentLimits1().getPermanentLimit());
        assertNull(createdModification.getCurrentLimits2().getPermanentLimit());
        assertEquals(1, createdModification.getCurrentLimits1().getTemporaryLimits().size());
        assertEquals(2, createdModification.getCurrentLimits2().getTemporaryLimits().size());

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    public void testCreateLineWithBothCurrentLimits() throws Exception {
        LineCreationInfos lineCreation = LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .seriesResistance(110.0)
                .seriesReactance(110.0)
                .shuntConductance1(15.0)
                .shuntSusceptance1(15.0)
                .shuntConductance2(25.0)
                .shuntSusceptance2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .currentLimits1(CurrentLimitsInfos.builder()
                    .permanentLimit(200.)
                    .temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").value(200.0).acceptableDuration(600).build()))
                    .build())
                .currentLimits2(CurrentLimitsInfos.builder()
                    .permanentLimit(100.)
                    .temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT20").value(600.0).acceptableDuration(1200).build()))
                    .build())
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();

        String lineCreationJson = mapper.writeValueAsString(lineCreation);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals(200., createdModification.getCurrentLimits1().getPermanentLimit(), 0.);
        assertEquals(100., createdModification.getCurrentLimits2().getPermanentLimit(), 0.);
        assertEquals(1, createdModification.getCurrentLimits1().getTemporaryLimits().size());
        assertEquals(1, createdModification.getCurrentLimits2().getTemporaryLimits().size());

        testNetworkModificationsCount(getGroupId(), 1);

        assertEquals(
            "LineCreationInfos(super=BranchCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, type=LINE_CREATION, date=null, stashed=null, messageType=null, messageValues=null), equipmentId=idLineEdited), equipmentName=nameLineEdited), seriesResistance=110.0, seriesReactance=110.0, voltageLevelId1=v2, voltageLevelId2=v1, busOrBusbarSectionId1=1A, busOrBusbarSectionId2=1.1, currentLimits1=CurrentLimitsInfos(permanentLimit=200.0, temporaryLimits=[CurrentTemporaryLimitCreationInfos(name=IT10, value=200.0, acceptableDuration=600)]), currentLimits2=CurrentLimitsInfos(permanentLimit=100.0, temporaryLimits=[CurrentTemporaryLimitCreationInfos(name=IT20, value=600.0, acceptableDuration=1200)]), connectionName1=cn1LineEdited, connectionDirection1=BOTTOM, connectionName2=cn2LineEdited, connectionDirection2=TOP, connectionPosition1=0, connectionPosition2=0), shuntConductance1=15.0, shuntSusceptance1=15.0, shuntConductance2=25.0, shuntSusceptance2=25.0)",
            lineCreation.toString()
        );
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineCreationInfos.builder()
                .equipmentId("idLine")
                .equipmentName("nameLine")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connectionName1("cn1Line")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2Line")
                .connectionDirection2(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .seriesResistance(110.0)
                .seriesReactance(110.0)
                .shuntConductance1(15.0)
                .shuntSusceptance1(15.0)
                .shuntConductance2(25.0)
                .shuntSusceptance2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build())
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLine("idLine"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLine("idLine"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "LINE_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"idLine\"}");
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "LINE_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"idLineEdited\"}");
    }
}
