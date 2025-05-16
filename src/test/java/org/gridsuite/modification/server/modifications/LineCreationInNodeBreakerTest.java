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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class LineCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Test
    void testCreateWithBadVariant() throws Exception {
        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        LineCreationInfos modificationToCreate = (LineCreationInfos) buildModification();
        modificationToCreate.setEquipmentId("idLine2");
        modificationToCreate.setEquipmentName("nameLine2");
        String modificationToCreateJson = getJsonBody(modificationToCreate, "variant_not_existing");
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(networkModificationsResult);
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().getFirst().isEmpty());  // no modifications returned
        assertNull(getNetwork().getLine("idLine2"));  // line was not created
        testNetworkModificationsCount(getGroupId(), 1);  // new modification stored in the database
    }

    @Test
    void testCreateWithErrors() throws Exception {
        LineCreationInfos lineCreationInfos = (LineCreationInfos) buildModification();
        lineCreationInfos.setEquipmentId("");
        String lineCreationInfosJson = getJsonBody(lineCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", ERROR_MESSAGE_KEY, reportService);

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        lineCreationInfosJson = getJsonBody(lineCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        lineCreationInfosJson = getJsonBody(lineCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setR(Double.NaN);
        lineCreationInfosJson = getJsonBody(lineCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("AC Line 'idLine4': r is invalid", ERROR_MESSAGE_KEY, reportService);

        lineCreationInfos.setR(100.0);
        lineCreationInfos.setX(Double.NaN);
        lineCreationInfosJson = getJsonBody(lineCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("AC Line 'idLine4': x is invalid", ERROR_MESSAGE_KEY, reportService);

        // try to create an existing line
        lineCreationInfos.setEquipmentId("line2");
        lineCreationInfosJson = getJsonBody(lineCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "line2").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateLineWithOnlyPermanentCurrentLimits() throws Exception {
        LineCreationInfos lineCreation = LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .r(110.0)
                .x(110.0)
                .g1(15.0)
                .b1(15.0)
                .g2(25.0)
                .b2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(5.).build()
                        ).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                                CurrentLimitsInfos.builder().permanentLimit(6.).build()
                        ).build()
                    )
                )
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();

        String lineCreationJson = getJsonBody(lineCreation, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals(5., createdModification.getOperationalLimitsGroups1().get(0).getCurrentLimits().getPermanentLimit(), 0.);
        assertEquals(6., createdModification.getOperationalLimitsGroups2().get(0).getCurrentLimits().getPermanentLimit(), 0.);
        assertTrue(createdModification.getOperationalLimitsGroups1().get(0).getCurrentLimits().getTemporaryLimits().isEmpty());
        assertTrue(createdModification.getOperationalLimitsGroups2().get(0).getCurrentLimits().getTemporaryLimits().isEmpty());

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    void testCreateLineWithOnlyTemporaryCurrentLimits() throws Exception {
        LineCreationInfos lineCreation = LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .r(110.0)
                .x(110.0)
                .g1(15.0)
                .b1(15.0)
                .g2(25.0)
                .b2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder()
                                .temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").value(200.0).acceptableDuration(600).build()))
                                .build()
                        ).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder()
                                .temporaryLimits(List.of(
                                    CurrentTemporaryLimitCreationInfos.builder().name("IT10").value(200.0).acceptableDuration(600).build(),
                                    CurrentTemporaryLimitCreationInfos.builder().name("IT20").value(100.0).acceptableDuration(1200).build()))
                                .build()
                            ).build()
                        )
                )
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();

        String lineCreationJson = getJsonBody(lineCreation, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertNull(createdModification.getOperationalLimitsGroups1().get(0).getCurrentLimits().getPermanentLimit());
        assertNull(createdModification.getOperationalLimitsGroups2().get(0).getCurrentLimits().getPermanentLimit());
        assertEquals(1, createdModification.getOperationalLimitsGroups1().get(0).getCurrentLimits().getTemporaryLimits().size());
        assertEquals(2, createdModification.getOperationalLimitsGroups2().get(0).getCurrentLimits().getTemporaryLimits().size());

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    void testCreateLineWithBothCurrentLimits() throws Exception {
        LineCreationInfos lineCreation = LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .r(110.0)
                .x(110.0)
                .g1(15.0)
                .b1(15.0)
                .g2(25.0)
                .b2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder()
                                .permanentLimit(200.)
                                .temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").value(200.0).acceptableDuration(600).build()))
                            .build()
                        ).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder()
                                .permanentLimit(100.)
                                .temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT20").value(600.0).acceptableDuration(1200).build()))
                            .build()
                        ).build()
                    )
                )
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .connected1(true)
                .connected2(false)
                .build();

        String lineCreationJson = getJsonBody(lineCreation, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals(200., createdModification.getOperationalLimitsGroups1().get(0).getCurrentLimits().getPermanentLimit(), 0.);
        assertEquals(100., createdModification.getOperationalLimitsGroups2().get(0).getCurrentLimits().getPermanentLimit(), 0.);
        assertEquals(1, createdModification.getOperationalLimitsGroups1().get(0).getCurrentLimits().getTemporaryLimits().size());
        assertEquals(1, createdModification.getOperationalLimitsGroups2().get(0).getCurrentLimits().getTemporaryLimits().size());

        testNetworkModificationsCount(getGroupId(), 1);

        assertEquals(
            "LineCreationInfos(super=BranchCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, type=LINE_CREATION, date=null, stashed=false, messageType=null, messageValues=null, activated=true), equipmentId=idLineEdited, properties=null), equipmentName=nameLineEdited), r=110.0, x=110.0, voltageLevelId1=v2, voltageLevelId2=v1, busOrBusbarSectionId1=1A, busOrBusbarSectionId2=1.1, operationalLimitsGroups1=[OperationalLimitsGroupInfos(id=null, currentLimits=CurrentLimitsInfos(permanentLimit=200.0, temporaryLimits=[CurrentTemporaryLimitCreationInfos(name=IT10, value=200.0, acceptableDuration=600)]))], operationalLimitsGroups2=[OperationalLimitsGroupInfos(id=null, currentLimits=CurrentLimitsInfos(permanentLimit=100.0, temporaryLimits=[CurrentTemporaryLimitCreationInfos(name=IT20, value=600.0, acceptableDuration=1200)]))], selectedOperationalLimitsGroup1=null, selectedOperationalLimitsGroup2=null, connectionName1=cn1LineEdited, connectionDirection1=BOTTOM, connectionName2=cn2LineEdited, connectionDirection2=TOP, connectionPosition1=0, connectionPosition2=0, connected1=true, connected2=false), g1=15.0, b1=15.0, g2=25.0, b2=25.0)",
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
                .stashed(false)
                .equipmentId("idLine")
                .equipmentName("nameLine")
                .r(100.0)
                .x(100.0)
                .g1(10.0)
                .b1(10.0)
                .g2(20.0)
                .b2(20.0)
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
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .r(110.0)
                .x(110.0)
                .g1(15.0)
                .b1(15.0)
                .g2(25.0)
                .b2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build()
                        ).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build()
                        ).build()
                    )
                )
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
        assertEquals(PROPERTY_VALUE, getNetwork().getLine("idLine").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLine("idLine"));
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLine", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLineEdited", updatedValues.get("equipmentId"));
    }
}
