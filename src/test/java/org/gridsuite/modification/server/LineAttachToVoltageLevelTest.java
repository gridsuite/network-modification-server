/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.utils.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.*;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.RestTemplate;
import java.util.*;
import java.util.stream.Collectors;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest
public class LineAttachToVoltageLevelTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final String CREATE_URI_STRING = "/v1/networks/{networkUuid}/line-attach?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String UPDATE_URI_STRING = "/v1/modifications/%s/line-attach-creation";
    private static final String DELETE_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "/modifications";
    private static final String DUPLICATE_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "?action=DUPLICATE";

    private static final  String NEW_LINE_NAME = "attachmentLine";
    private static final  String NEW_VL_NAME = "AttPointId";
    private static final  String NEW_ADDITIONAL_VL_NAME = "vl1";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper mapper;
    @MockBean
    private NetworkStoreService networkStoreService;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @MockBean
    @Qualifier("reportServer")
    private RestTemplate reportServerRest;

    @Autowired
    private NetworkModificationService networkModificationService;

    private ObjectWriter objectWriter;
    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });
        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
            .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
    }

    @After
    public void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
    }

    private LineCreationInfos getAttachmentLine() {
        return LineCreationInfos.builder()
                .equipmentId(NEW_LINE_NAME)
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();
    }

    private VoltageLevelCreationInfos getNewVoltageLevel() {
        return VoltageLevelCreationInfos.builder()
                .equipmentId(NEW_ADDITIONAL_VL_NAME)
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
                .busbarConnections(List.of())
                .build();
    }

    private List<LineAttachToVoltageLevelInfos> getModifications() {
        return modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(LineAttachToVoltageLevelInfos.class::cast).collect(Collectors.toList());
    }

    private void createWithExistingVoltageLevel() throws Exception {
        LineAttachToVoltageLevelInfos lineAttachToVL = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line3")
                .percent(10.0)
                .attachmentPointId(NEW_VL_NAME)   // created VL
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("v4")     // use existing VL
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())   // created Line
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();

        String lineAttachToVLJson = objectWriter.writeValueAsString(lineAttachToVL);
        mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(lineAttachToVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        // new equipments in the network:
        assertNotNull(network.getLine(NEW_LINE_NAME));
        assertNotNull(network.getLine("nl1"));
        assertNotNull(network.getLine("nl2"));
        assertNotNull(network.getVoltageLevel(NEW_VL_NAME));
    }

    private void createWithNewVoltageLevel() throws Exception {
        LineAttachToVoltageLevelInfos lineAttachToWithNewVL = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line3")
                .percent(20.0)
                .attachmentPointId(NEW_VL_NAME)
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())  // create another new VL
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
        String lineAttachToWithNewVLJson = objectWriter.writeValueAsString(lineAttachToWithNewVL);
        mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(lineAttachToWithNewVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        // new equipments in the network:
        assertNotNull(network.getLine(NEW_LINE_NAME));
        assertNotNull(network.getLine("nl1"));
        assertNotNull(network.getLine("nl2"));
        assertNotNull(network.getVoltageLevel(NEW_VL_NAME));
        assertNotNull(network.getVoltageLevel(NEW_ADDITIONAL_VL_NAME));
    }

    @Test
    public void testCreateWithExistingVoltageLevel() throws Exception {
        createWithExistingVoltageLevel();
        assertEquals(1, getModifications().size());
    }

    @Test
    public void testCreateWithNewVoltageLevel() throws Exception {
        createWithNewVoltageLevel();
        assertEquals(1, getModifications().size());
    }

    @Test
    public void testUpdate() throws Exception {
        // 1- First create a modification
        createWithExistingVoltageLevel();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LineAttachToVoltageLevelInfos firstModification = modifications.get(0);
        assertEquals("line3", firstModification.getLineToAttachToId());
        assertEquals("attPointName", firstModification.getAttachmentPointName());
        assertEquals(NEW_VL_NAME, firstModification.getAttachmentPointId());
        assertEquals(10., firstModification.getPercent(), 0.);

        // 2- Then update it
        LineAttachToVoltageLevelInfos lineAttachWithNewVLUpd = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line2")
                .percent(30.0)
                .attachmentPointId(NEW_VL_NAME)
                .attachmentPointName("newAttPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())
                .newLine1Id("newLine1Id")
                .newLine1Name("newLine1Name")
                .newLine2Id("newLine2Id")
                .newLine2Name("newLine2Name")
                .build();
        String lineAttachWithNewVLUpdJson = objectWriter.writeValueAsString(lineAttachWithNewVLUpd);
        mockMvc.perform(put(String.format(UPDATE_URI_STRING, firstModification.getUuid())).content(lineAttachWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        modifications = getModifications();
        assertEquals(1, modifications.size());
        firstModification = modifications.get(0);
        // modification has changed
        assertEquals(30., firstModification.getPercent(), 0.);
        assertEquals("newLine1Id", firstModification.getNewLine1Id());
        assertEquals("newLine2Id", firstModification.getNewLine2Id());
        assertEquals("newLine1Name", firstModification.getNewLine1Name());
        assertEquals("newLine2Name", firstModification.getNewLine2Name());
        assertEquals("newAttPointName", firstModification.getAttachmentPointName());
    }

    @Test
    public void testDeletion() throws Exception {
        // 1- First create a modification
        createWithExistingVoltageLevel();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LineAttachToVoltageLevelInfos firstModification = modifications.get(0);

        // 2- then remove it
        mockMvc.perform(delete(DELETE_URI_STRING).queryParam("modificationsUuids", firstModification.getUuid().toString())).andExpect(status().isOk());
        assertEquals(0, getModifications().size());
    }

    @Test
    public void testDuplicate() throws Exception {
        // 1- First create 2 modifications
        createWithExistingVoltageLevel();
        createWithNewVoltageLevel();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(2, modifications.size());
        assertEquals(1, modifications.stream().filter(r -> r.getPercent() == 10.).count());
        assertEquals(1, modifications.stream().filter(r -> r.getPercent() == 20.).count());

        // 2- Then duplicate them in current group
        List<UUID> duplicateModificationUuidList = modifications.stream().map(LineAttachToVoltageLevelInfos::getUuid).collect(Collectors.toList());
        mockMvc.perform(put(DUPLICATE_URI_STRING)
                        .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        modifications = getModifications();
        assertEquals(4, modifications.size());
        assertEquals(2, modifications.stream().filter(r -> r.getPercent() == 10.).count());
        assertEquals(2, modifications.stream().filter(r -> r.getPercent() == 20.).count());
    }

    @Test
    public void testCreateErrorLineNotFound() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LineAttachToVoltageLevelInfos lineAttachToAbsentLine = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("absent_line_id")
                .percent(0.0)
                .attachmentPointId(NEW_VL_NAME)
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("v4")
                .bbsOrBusId("v1bbs")
                .attachmentLine(getAttachmentLine())
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();

        String lineAttachToAbsentLineJson = objectWriter.writeValueAsString(lineAttachToAbsentLine);
        mvcResult = mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage());
    }

    @Test
    public void testCreateErrorMissingLine() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LineAttachToVoltageLevelInfos incompleteLineAttachToVL = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line3")
                .percent(0.0)
                .attachmentPointId(NEW_VL_NAME)
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("v4")
                .bbsOrBusId("1.A")
                .attachmentLine(null)  // we omit a mandatory input data
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();

        String incompleteLineAttachToVLJson = objectWriter.writeValueAsString(incompleteLineAttachToVL);
        mvcResult = mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(incompleteLineAttachToVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(LINE_ATTACH_ERROR, "Missing required attachment line description").getMessage());
    }

    @Test
    public void testUpdateErrorModificationNotFound() throws Exception {
        LineAttachToVoltageLevelInfos lineAttachWithNewVLUpd = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line2")
                .percent(30.0)
                .attachmentPointId(NEW_VL_NAME)
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine())
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
        String lineAttachWithNewVLUpdJson = objectWriter.writeValueAsString(lineAttachWithNewVLUpd);
        UUID uuidNotFound = UUID.randomUUID();
        MvcResult mvcResult = mockMvc.perform(put(String.format(UPDATE_URI_STRING, uuidNotFound)).content(lineAttachWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", uuidNotFound)).getMessage());
    }
}
