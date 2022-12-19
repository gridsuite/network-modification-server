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
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
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

    private static final  String NEW_LINE_NAME = "attachmentLine";
    private static final  String NEW_VL_NAME = "AttPointId";
    private static final  String NEW_ADDITIONAL_VL_NAME = "vl1";

    private static final String COPY_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY";
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_GET_PUT = URI_NETWORK_MODIF_BASE + "/";
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;

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

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    private ObjectWriter objectWriter;
    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> network);
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

    private LineAttachToVoltageLevelInfos getLineAttachToVoltageLevelModification(UUID modificationUuid) {
        return (LineAttachToVoltageLevelInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private LineCreationInfos getAttachmentLine(String lineName) {
        return LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId(lineName)
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();
    }

    private VoltageLevelCreationInfos getNewVoltageLevel(String vlName) {
        return VoltageLevelCreationInfos.builder()
                .type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .equipmentId(vlName)
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

    private LineAttachToVoltageLevelInfos getCreateWithExistingVoltageLevelInfos() {
        return LineAttachToVoltageLevelInfos.builder()
            .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
            .lineToAttachToId("line3")
            .percent(10.0)
            .attachmentPointId(NEW_VL_NAME)   // created VL
            .attachmentPointName("attPointName")
            .mayNewVoltageLevelInfos(null)
            .existingVoltageLevelId("v4")     // use existing VL
            .bbsOrBusId("1.A")
            .attachmentLine(getAttachmentLine(NEW_LINE_NAME))   // created Line
            .newLine1Id("nl1")
            .newLine1Name("NewLine1")
            .newLine2Id("nl2")
            .newLine2Name("NewLine2")
            .build();
    }

    private void checkCreationWithExistingVoltageLevel(LineAttachToVoltageLevelInfos modification) {
        assertNotNull(modification);
        assertEquals("line3", modification.getLineToAttachToId());
        assertEquals(10., modification.getPercent(), 0.);
        assertEquals("AttPointId", modification.getAttachmentPointId());
        assertEquals("attPointName", modification.getAttachmentPointName());
        assertNull(modification.getMayNewVoltageLevelInfos());
        assertEquals("v4", modification.getExistingVoltageLevelId());
        assertEquals("1.A", modification.getBbsOrBusId());
        assertEquals("attachmentLine", modification.getAttachmentLine().getEquipmentId());
        assertEquals("nl1", modification.getNewLine1Id());
        assertEquals("nl2", modification.getNewLine2Id());
        assertEquals("NewLine1", modification.getNewLine1Name());
        assertEquals("NewLine2", modification.getNewLine2Name());
    }

    private void saveEntityWithExistingVoltageLevel() {
        // this is a creation using directly the Repository
        LineAttachToVoltageLevelInfos lineAttachToVL = getCreateWithExistingVoltageLevelInfos();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lineAttachToVL.toEntity()));
    }

    private void checkCreationWithNewVoltageLevel(LineAttachToVoltageLevelInfos modification) {
        assertNotNull(modification);
        assertEquals("line3", modification.getLineToAttachToId());
        assertEquals(20., modification.getPercent(), 0.);
        assertEquals("AttPointId", modification.getAttachmentPointId());
        assertEquals("attPointName", modification.getAttachmentPointName());
        assertNull(modification.getExistingVoltageLevelId());
        assertEquals("vl1", modification.getMayNewVoltageLevelInfos().getEquipmentId());
        assertEquals("1.A", modification.getBbsOrBusId());
        assertEquals("attachmentLine", modification.getAttachmentLine().getEquipmentId());
        assertEquals("nl1", modification.getNewLine1Id());
        assertEquals("nl2", modification.getNewLine2Id());
        assertEquals("NewLine1", modification.getNewLine1Name());
        assertEquals("NewLine2", modification.getNewLine2Name());
    }

    @Test
    public void testModificationRepository() {
        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId("attachmentLineId")
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();

        LineAttachToVoltageLevelEntity lineAttachToEntity1 = LineAttachToVoltageLevelEntity.toEntity(
                "lineId0", 40.0, "AttachmentPointId", null, null, "vl1", "bbsId", attachmentLine, "line1Id", "line1Name", "line2Id", "line2Name"
        );
        VoltageLevelCreationInfos voltageLevelCreationInfos = TestUtils.makeAVoltageLevelInfos(1, 0);
        LineAttachToVoltageLevelEntity lineAttachToEntity2 = LineAttachToVoltageLevelEntity.toEntity(
                "lineId1", 40.0, "AttachmentPointId", null, voltageLevelCreationInfos, null, "bbsId", attachmentLine, "line1Id", "line1Name", "line2Id", "line2Name"
        );
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lineAttachToEntity1, lineAttachToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(0).getUuid()),
                MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos(
                        lineAttachToEntity1.toLineAttachToVoltageLevelInfos()));

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(1).getUuid()),
                MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos(
                        lineAttachToEntity2.toLineAttachToVoltageLevelInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(lineAttachToEntity1.getId(),
                lineAttachToEntity2.getId()));
        TestUtils.assertRequestsCount(2, 0, 0, 12);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        TestUtils.assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testCreateWithExistingVoltageLevel() throws Exception {
        LineAttachToVoltageLevelInfos lineAttachToVL = getCreateWithExistingVoltageLevelInfos();

        assertNotNull(network.getLine("line3"));

        String lineAttachToVLJson = objectWriter.writeValueAsString(lineAttachToVL);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(lineAttachToVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        checkCreationWithExistingVoltageLevel(modifications.get(0));

        // new equipments in the network:
        assertNotNull(network.getLine(NEW_LINE_NAME));
        assertNotNull(network.getLine("nl1"));
        assertNotNull(network.getLine("nl2"));
        assertNotNull(network.getVoltageLevel(NEW_VL_NAME));
        // replaced line is gone
        assertNull(network.getLine("line3"));
    }

    @Test
    public void testCreateWithNewVoltageLevel() throws Exception {
        LineAttachToVoltageLevelInfos lineAttachToWithNewVL = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line3")
                .percent(20.0)
                .attachmentPointId(NEW_VL_NAME)
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel(NEW_ADDITIONAL_VL_NAME))  // create another new VL
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine(NEW_LINE_NAME))
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();

        assertNotNull(network.getLine("line3"));

        String lineAttachToWithNewVLJson = objectWriter.writeValueAsString(lineAttachToWithNewVL);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(lineAttachToWithNewVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        checkCreationWithNewVoltageLevel(modifications.get(0));

        // new equipments in the network:
        assertNotNull(network.getLine(NEW_LINE_NAME));
        assertNotNull(network.getLine("nl1"));
        assertNotNull(network.getLine("nl2"));
        assertNotNull(network.getVoltageLevel(NEW_VL_NAME));
        assertNotNull(network.getVoltageLevel(NEW_ADDITIONAL_VL_NAME));
        // replaced line is gone
        assertNull(network.getLine("line3"));
    }

    @Test
    public void testUpdate() throws Exception {
        // 1- First create a modification in database
        saveEntityWithExistingVoltageLevel();

        List<LineAttachToVoltageLevelInfos> modificationsAfterCreate = getModifications();
        assertEquals(1, modificationsAfterCreate.size());
        UUID modificationToUpdate = modificationsAfterCreate.get(0).getUuid();

        // 2- Then update it
        LineAttachToVoltageLevelInfos lineAttachWithNewVLUpd = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachToId("line2")
                .percent(30.0)
                .attachmentPointId("newAttPointId")
                .attachmentPointName("newAttPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel("newVlName"))
                .existingVoltageLevelId(null)
                .bbsOrBusId("2.A")
                .attachmentLine(getAttachmentLine("newLineName"))
                .newLine1Id("newLine1Id")
                .newLine1Name("newLine1Name")
                .newLine2Id("newLine2Id")
                .newLine2Name("newLine2Name")
                .build();
        String lineAttachWithNewVLUpdJson = objectWriter.writeValueAsString(lineAttachWithNewVLUpd);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationToUpdate).content(lineAttachWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LineAttachToVoltageLevelInfos firstModification = modifications.get(0);
        // modification has changed
        assertEquals("line2", firstModification.getLineToAttachToId());
        assertEquals(30., firstModification.getPercent(), 0.);
        assertEquals("newAttPointId", firstModification.getAttachmentPointId());
        assertEquals("newAttPointName", firstModification.getAttachmentPointName());
        assertEquals("newVlName", firstModification.getMayNewVoltageLevelInfos().getEquipmentId());
        assertEquals("2.A", firstModification.getBbsOrBusId());
        assertEquals("newLineName", firstModification.getAttachmentLine().getEquipmentId());
        assertEquals("newLine1Id", firstModification.getNewLine1Id());
        assertEquals("newLine2Id", firstModification.getNewLine2Id());
        assertEquals("newLine1Name", firstModification.getNewLine1Name());
        assertEquals("newLine2Name", firstModification.getNewLine2Name());
    }

    @Test
    public void testDelete() throws Exception {
        // 1- First create a modification in database
        saveEntityWithExistingVoltageLevel();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LineAttachToVoltageLevelInfos firstModification = modifications.get(0);

        // 2- then remove it
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", firstModification.getUuid().toString()))
                .andExpect(status().isOk());

        assertEquals(0, getModifications().size());
    }

    @Test
    public void testCopy() throws Exception {
        // 1- First create a modification in database
        saveEntityWithExistingVoltageLevel();

        List<LineAttachToVoltageLevelInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        assertEquals(1, modifications.stream().filter(r -> r.getPercent() == 10.).count());

        // 2- Then copy it in current group
        List<UUID> copyModificationUuidList = modifications.stream().map(LineAttachToVoltageLevelInfos::getUuid).collect(Collectors.toList());
        mockMvc.perform(put(COPY_URI_STRING)
                        .content(objectWriter.writeValueAsString(copyModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        modifications = getModifications();
        assertEquals(2, modifications.size());
        checkCreationWithExistingVoltageLevel(modifications.get(0));
        checkCreationWithExistingVoltageLevel(modifications.get(1));
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
                .attachmentLine(getAttachmentLine(NEW_LINE_NAME))
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();

        String lineAttachToAbsentLineJson = objectWriter.writeValueAsString(lineAttachToAbsentLine);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
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
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(incompleteLineAttachToVLJson).contentType(MediaType.APPLICATION_JSON))
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
                .mayNewVoltageLevelInfos(getNewVoltageLevel(NEW_ADDITIONAL_VL_NAME))
                .existingVoltageLevelId(null)
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine(NEW_LINE_NAME))
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
        String lineAttachWithNewVLUpdJson = objectWriter.writeValueAsString(lineAttachWithNewVLUpd);
        UUID uuidNotFound = UUID.randomUUID();
        MvcResult mvcResult = mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + uuidNotFound).content(lineAttachWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", uuidNotFound)).getMessage());
    }
}
