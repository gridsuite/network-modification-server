package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteVoltageLevelOnLineEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.utils.MatcherDeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
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
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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
    private EquipmentInfosService equipmentInfosService;
    private ObjectWriter objectWriter;
    private Network network;
    @Autowired
    private NetworkModificationRepository networkModificationRepository;

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
        equipmentInfosService.deleteAll();
        SQLStatementCountValidator.reset();
    }

    @Test
    public void testCreateDeleteVoltageLevelOnLine() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        String deleteVoltageLevelOnLineUriString = "/v1/networks/{networkUuid}/delete-voltage-level-on-line?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
        // test create
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = new DeleteVoltageLevelOnLineInfos("line1", "line2", "line3", "nl4", "NewLine4");
        String deleteVoltageLevelOnLineInfosJson = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mvcResult = mockMvc.perform(post(deleteVoltageLevelOnLineUriString, TEST_NETWORK_ID).content(deleteVoltageLevelOnLineInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();

        List<EquipmentModificationInfos> result = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertNotNull(result);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateDeleteVoltageLevelOnLineWithIncorrectIdLine() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        String deleteVoltageLevelOnLineUriString = "/v1/networks/{networkUuid}/delete-voltage-level-on-line?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnAbsentLineInfos = new DeleteVoltageLevelOnLineInfos("absent_line_id", "line2", "line3", "nl4", "NewLine4");

        String deleteVoltageLevelOnAbsentLineInfosJson = objectWriter.writeValueAsString(deleteVoltageLevelOnAbsentLineInfos);
        // test create with absent_line_id
        mvcResult = mockMvc.perform(post(deleteVoltageLevelOnLineUriString, TEST_NETWORK_ID).content(deleteVoltageLevelOnAbsentLineInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        //assertEquals("LINE_NOT_FOUND : Line absent_line_id is not found", resultAsString);
    }

    @Test
    public void testUpdateDeleteVoltageLevelOnLineCreation() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        String deleteVoltageLevelOnLineUriString = "/v1/networks/{networkUuid}/delete-voltage-level-on-line?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder().type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE).lineToAttachTo1Id("line1").lineToAttachTo2Id("line2").attachedLineId("line3").replacingLine1Id("nl4").replacingLine1Name("NewLine4").build();
        String deleteVoltageLevelOnLineInfosJson = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mvcResult = mockMvc.perform(post(deleteVoltageLevelOnLineUriString, TEST_NETWORK_ID).content(deleteVoltageLevelOnLineInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> result = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertNotNull(result);
        Optional<EquipmentModificationInfos> deleteVoltageLevelOnLine = result.stream().filter(r -> r.getType() == ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE).findFirst();
        assertTrue(deleteVoltageLevelOnLine.isPresent());

        var listModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        // test update
        mockMvc.perform(put("/v1/modifications/" + listModifications.get(0).getUuid() + "/delete-voltage-level-on-line-creation").content(deleteVoltageLevelOnLineInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testDuplicateDeleteVoltageLevelOnLine() throws Exception {
        //create Delete a voltage level on a line
        String deleteVoltageLevelOnLineUriString = "/v1/networks/{networkUuid}/delete-voltage-level-on-line?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = new DeleteVoltageLevelOnLineInfos("line1", "line2", "line3", "replacingLineId1", "replacingLine1");

        mockMvc.perform(
                        post(deleteVoltageLevelOnLineUriString, TEST_NETWORK_ID)
                                .content(objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        //test copy group with Delete a voltage level on a line
        UUID newGroupUuid = UUID.randomUUID();
        String uriString = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(uriString)).andExpect(status().isOk());

        testNetworkModificationsCount(newGroupUuid, 1);
    }

    @Test
    public void testRepositoryOperationsOnDeleteVoltageLevelOnLine() {
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

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        // get all modifications for the given group of a network
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications?onlyMetadata=true", groupUuid).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> modificationsTestGroupId = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(actualSize, modificationsTestGroupId.size());
    }

    private DeleteVoltageLevelOnLineInfos getDeleteVoltageLevelOnLineModification(UUID modificationUuid) {
        return (DeleteVoltageLevelOnLineInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

}
