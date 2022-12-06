package org.gridsuite.modification.server;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteVoltageLevelOnLineEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.utils.MatcherDeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
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
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest()
public class DeleteVoltageLevelOnLineTest {
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();
    public static final String CREATE_URI_STRING = "/v1/networks/{networkUuid}/delete-voltage-level-on-line?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String UPDATE_URI_STRING = "/v1/modifications/%s/delete-voltage-level-on-line-creation";
    private static final String DELETE_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "/modifications";
    private static final String COPY_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY";

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
        SQLStatementCountValidator.reset();
    }

    @After
    public void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
    }

    @Test
    public void createTest() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // test create
        createDeleteVoltageLevelOnLine();
        // test create with absent_line_id
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnAbsentLineInfos = new DeleteVoltageLevelOnLineInfos("absent_line_id", "line2", "line3", "nl4", "NewLine4");
        String deleteVoltageLevelOnAbsentLineInfosJson = objectWriter.writeValueAsString(deleteVoltageLevelOnAbsentLineInfos);
        mvcResult = mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(deleteVoltageLevelOnAbsentLineInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        //assertEquals("LINE_NOT_FOUND : Line absent_line_id is not found", resultAsString);
    }

    @Test
    public void testUpdate() throws Exception {
        // 1- First create a modification
        createDeleteVoltageLevelOnLine();

        List<DeleteVoltageLevelOnLineInfos> modificationsAfterCreate = getModifications();
        assertEquals(1, modificationsAfterCreate.size());
        UUID modificationToUpdate = modificationsAfterCreate.get(0).getUuid();

        // 2- Then update it
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachTo1Id("line00")
                .lineToAttachTo2Id("line11")
                .attachedLineId("line22")
                .replacingLine1Id("replacingLine2")
                .replacingLine1Name("replacingLine2")
                .build();
        String json = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mockMvc.perform(put(String.format(UPDATE_URI_STRING, modificationToUpdate)).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        DeleteVoltageLevelOnLineInfos firstModification = modifications.get(0);
        // modification has changed
        assertEquals("line00", firstModification.getLineToAttachTo1Id());
        assertEquals("line11", firstModification.getLineToAttachTo2Id());
        assertEquals("line22", firstModification.getAttachedLineId());
        assertEquals("replacingLine2", firstModification.getReplacingLine1Id());
        assertEquals("replacingLine2", firstModification.getReplacingLine1Name());
    }

    @Test
    public void testDelete() throws Exception {
        // 1- First create a modification
        createDeleteVoltageLevelOnLine();

        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        DeleteVoltageLevelOnLineInfos firstModification = modifications.get(0);

        // 2- then remove it
        mockMvc.perform(delete(DELETE_URI_STRING).queryParam("modificationsUuids", firstModification.getUuid().toString())).andExpect(status().isOk());
        assertEquals(0, getModifications().size());
    }

    @Test
    public void testCopy() throws Exception {
        // 1- First create 1 modification
        createDeleteVoltageLevelOnLine();

        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());

        // 2- Then copy them in current group
        List<UUID> copyModificationUuidList = modifications.stream().map(DeleteVoltageLevelOnLineInfos::getUuid).collect(Collectors.toList());
        mockMvc.perform(put(COPY_URI_STRING)
                        .content(objectWriter.writeValueAsString(copyModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        modifications = getModifications();
        assertEquals(2, modifications.size());
    }

    @Test
    public void testModificationRepository() {
        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity1 = DeleteVoltageLevelOnLineEntity.toEntity(
                "lineId0", "lineId1", "lineId3", "line1Id", "line1Name");
        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity2 = DeleteVoltageLevelOnLineEntity.toEntity(
                "lineId4", "lineId5", "lineId6", "line3Id", "line3Name");
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(deleteVoltageLevelOnLineToEntity1, deleteVoltageLevelOnLineToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(0).getUuid()),
                MatcherDeleteVoltageLevelOnLineInfos.createMatcherDeleteVoltageLevelOnLineInfos(
                        deleteVoltageLevelOnLineToEntity1.toModificationInfos()));

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(1).getUuid()),
                MatcherDeleteVoltageLevelOnLineInfos.createMatcherDeleteVoltageLevelOnLineInfos(
                        deleteVoltageLevelOnLineToEntity2.toModificationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(deleteVoltageLevelOnLineToEntity1.getId(),
                deleteVoltageLevelOnLineToEntity2.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    private void createDeleteVoltageLevelOnLine() throws Exception {
        DeleteVoltageLevelOnLineInfos lineAttachToVL = DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
                .lineToAttachTo1Id("line1")
                .lineToAttachTo2Id("line2")
                .attachedLineId("line3")
                .replacingLine1Id("replacingLine1")
                .replacingLine1Name("replacingLine1")
                .build();

        String json = objectWriter.writeValueAsString(lineAttachToVL);
        mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        // new equipments in the network:
        assertNotNull(network.getLine("line1"));
        assertNotNull(network.getLine("line2"));
        assertNotNull(network.getLine("line3"));
    }

    private List<DeleteVoltageLevelOnLineInfos> getModifications() {
        return modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(DeleteVoltageLevelOnLineInfos.class::cast).collect(Collectors.toList());
    }

    private DeleteVoltageLevelOnLineInfos getDeleteVoltageLevelOnLineModification(UUID modificationUuid) {
        return (DeleteVoltageLevelOnLineInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

}
