/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.LoadCreationInfos.LoadCreationInfosBuilder;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.modifications.ModificationUtils;
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
import org.springframework.web.util.NestedServletException;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest(properties = {"test.elasticsearch.enabled=true"})
public class ModificationControllerTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_ID_2 = UUID.fromString("7928181e-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_WITH_TEE_POINT_ID = UUID.fromString("1928181e-7974-4592-ba19-88027e4254e4");
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");
    private static final UUID TEST_NETWORK_WITH_FLUSH_ERROR_ID = UUID.fromString("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_NETWORK_BUS_BREAKER_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_MIXED_TOPOLOGY_ID = UUID.fromString("22222222-7977-4592-ba19-88027e4254e4");
    public static final String VARIANT_NOT_EXISTING_ID = "variant_not_existing";
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_GET_PUT = URI_NETWORK_MODIF_BASE + "/";
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_BUS_BREAKER = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_BUS_BREAKER_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_FULL_MIXED_TOPO = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_MIXED_TOPOLOGY_ID + "&groupUuid=" + TEST_NETWORK_MIXED_TOPOLOGY_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF_BAD_NETWORK = URI_NETWORK_MODIF_BASE + "?networkUuid=" + NOT_FOUND_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_BAD_VARIANT = URI_NETWORK_MODIF + "&variantId=" + VARIANT_NOT_EXISTING_ID;

    private static final String URI_NETWORK_WITH_TEE_POINT_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_WITH_TEE_POINT_ID + URI_NETWORK_MODIF_PARAMS;

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

    private Network network2;

    private Network networkWithTeePoint;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });
        when(networkStoreService.getNetwork(TEST_NETWORK_ID_2)).then((Answer<Network>) invocation -> {
            network2 = NetworkCreation.create(TEST_NETWORK_ID_2, false);
            return network2;
        });

        networkWithTeePoint = NetworkWithTeePoint.create(TEST_NETWORK_WITH_TEE_POINT_ID);
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_TEE_POINT_ID)).then((Answer<Network>) invocation -> networkWithTeePoint);

        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_FLUSH_ERROR_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_WITH_FLUSH_ERROR_ID, true));
        when(networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID)).then((Answer<Network>) invocation -> NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID));
        when(networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID)).then((Answer<Network>) invocation -> NetworkCreation.createMixedTopology(TEST_NETWORK_MIXED_TOPOLOGY_ID));

        doThrow(new PowsyblException()).when(networkStoreService).flush(argThat(n -> TEST_NETWORK_WITH_FLUSH_ERROR_ID.toString().equals(n.getId())));

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
            .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @After
    public void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @Test
    public void testModificationException() {
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR).getMessage(), MODIFICATION_ERROR.name());
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, "Error message").getMessage(), MODIFICATION_ERROR.name() + " : Error message");
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, new IllegalArgumentException("Error message")).getMessage(), MODIFICATION_ERROR.name() + " : Error message");
    }

    @Test
    public void testEquipmentIdNonNull() {
        String errorMessage = "equipmentId is marked non-null but is null";
        LoadCreationInfosBuilder<?, ?> loadCreationBuilder = LoadCreationInfos.builder();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, loadCreationBuilder::build).getMessage());
        LoadCreationInfosBuilder<?, ?> loadCreationBuilder1 = loadCreationBuilder.equipmentId(null);
        assertEquals(errorMessage, assertThrows(NullPointerException.class, loadCreationBuilder1::build).getMessage());
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().type(ModificationType.LOAD_CREATION).equipmentId("idLoad").build();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationInfos.setEquipmentId(null)).getMessage());
    }

    @SneakyThrows
    @Test
    public void testNetworkNotFound() {
        mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK)
            .content(objectWriter.writeValueAsString(LoadCreationInfos.builder().type(ModificationType.LOAD_CREATION).equipmentId("id").build()))
            .contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage())
            );
    }

    @Test
    public void assertThrowsUpdateModificationNotFound() {
        UUID modificationUuid = UUID.randomUUID();
        ModificationInfos modificationInfos = LoadCreationInfos.builder().type(ModificationType.LOAD_CREATION).equipmentId("id").build();
        String errorMessage = assertThrows(NetworkModificationException.class, () -> networkModificationService.updateModification(modificationUuid, modificationInfos)).getMessage();
        assertEquals(new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)).getMessage(), errorMessage);
        assertThrows(NullPointerException.class, () -> networkModificationService.updateModification(modificationUuid, null));
    }

    @Test
    public void testModificationGroups() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);

        // no groups
        mvcResult = mockMvc.perform(get("/v1/groups")).andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON)).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<UUID> bsicListResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(bsicListResult, List.of());
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultModificationInfos.get(0), createMatcherEquipmentAttributeModificationInfos("v1b1", Set.of("s1"), "open", true, IdentifiableType.SWITCH));

         // switch opening to create the default group
        mvcResult = mockMvc.perform(get("/v1/groups")).andExpectAll(
         status().isOk(),
         content().contentType(MediaType.APPLICATION_JSON))
         .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<UUID> bsicListResultUUID = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(bsicListResultUUID, List.of(TEST_GROUP_ID));
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications", TEST_GROUP_ID)).andExpectAll(
         status().isOk(),
         content().contentType(MediaType.APPLICATION_JSON))
         .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsicListResulModifInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(1, bsicListResulModifInfos.size());
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications?onlyMetadata=true", TEST_GROUP_ID))
                        .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                        .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsicListResultInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(1, bsicListResultInfos.size());

        // delete the default modification group of a network
        mockMvc.perform(delete("/v1/groups/{groupUuid}", TEST_GROUP_ID))
                .andExpect(status().isOk());

        mockMvc.perform(get("/v1/groups/{groupUuid}/modifications?onlyMetadata=true", TEST_GROUP_ID)).andExpectAll(status().isNotFound(),
                    content().string(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage()));

        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications?onlyMetadata=true&errorOnGroupNotFound=false", TEST_GROUP_ID)).andExpectAll(
         status().isOk(),
         content().contentType(MediaType.APPLICATION_JSON))
         .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsicListModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(bsicListModificationInfos, List.of());
    }

    @Test
    public void testDeleteModification() throws Exception {
        MvcResult mvcResult;
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);

        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        List<ModificationInfos> modifications = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(modifications);
        assertEquals(1, modifications.size());

        modifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modifications.size());
        String uuidString = modifications.get(0).getUuid().toString();
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", UUID.randomUUID().toString())
                        .queryParam("uuids", uuidString))
                .andExpect(status().isNotFound());

        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", uuidString))
                .andExpect(status().isOk());

        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, false, true).size());

        /* non existing modification */
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", uuidString))
                .andExpect(status().isNotFound());
        mockMvc.perform(delete("/v1/groups/" + TEST_GROUP_ID)).andExpect(status().isOk());
        mockMvc.perform(delete("/v1/groups/" + TEST_GROUP_ID)).andExpect(status().isNotFound());
        mockMvc.perform(delete("/v1/groups/" + TEST_GROUP_ID).queryParam("errorOnGroupNotFound", "false")).andExpect(status().isOk());
    }

    @Test
    public void testLineStatusModification() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        BranchStatusModificationInfos branchStatusModificationInfos = BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS)
                .equipmentId("line2")
                .action(BranchStatusModificationInfos.ActionType.LOCKOUT)
                .build();
        String branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);

        // line lockout
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<BranchStatusModificationInfos> bsmListResultBranchStatus = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultBranchStatus.get(0), createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.LOCKOUT, Set.of("s1", "s2")));

        // line switch on (already switched on)
        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.SWITCH_ON);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<BranchStatusModificationInfos> bsmListResultBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultBranchStatusInfos.get(0), createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.SWITCH_ON, Set.of()));

        // line trip
        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.TRIP);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<BranchStatusModificationInfos> bsmlrBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrBranchStatusInfos.get(0), createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.TRIP, Set.of("s1", "s2")));

        branchStatusModificationInfos.setEquipmentId("line3");
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<BranchStatusModificationInfos> bsmlResultBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(1, bsmlResultBranchStatusInfos.size());
        var matcher = createMatcherBranchStatusModificationInfos("line3", BranchStatusModificationInfos.ActionType.TRIP, Set.of("s1", "s2"));
        assertTrue(bsmlResultBranchStatusInfos.stream().anyMatch(matcher::matchesSafely));

        // line energise on one end
        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE);
        branchStatusModificationInfos.setEquipmentId("line2");
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<BranchStatusModificationInfos> bsmlrbsInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbsInfos.get(0), createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE, Set.of("s2")));

        // line energise on other end
        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<BranchStatusModificationInfos> bsmlrbStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbStatusInfos.get(0), createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO, Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 6);
    }

    @Test
    public void testLineStatusModificationWithErrors() throws Exception {

        BranchStatusModificationInfos branchStatusModificationInfos = BranchStatusModificationInfos.builder()
                .type(ModificationType.BRANCH_STATUS)
                .equipmentId("line2")
                .action(BranchStatusModificationInfos.ActionType.LOCKOUT)
                .build();
        String branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);

        // network not existing
        mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage()));

        // line not existing
        branchStatusModificationInfos.setEquipmentId("notFound");
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(LINE_NOT_FOUND, "notFound").getMessage()));

        // modification action empty
        branchStatusModificationInfos.setEquipmentId("line2");
        branchStatusModificationInfos.setAction(null);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY).getMessage()));

        // modification action not existing
        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.LOCKOUT);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        branchStatusModificationInfosJson = branchStatusModificationInfosJson.replace("LOCKOUT", "INVALID_ACTION");
        mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError());

        branchStatusModificationInfos.setEquipmentId("line3");
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends").getMessage()));

        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise line end").getMessage()));

        branchStatusModificationInfos.setAction(BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO);
        branchStatusModificationInfosJson = objectWriter.writeValueAsString(branchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(branchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise line end").getMessage()));
    }

    @Test
    public void testUndoModificationsOnNetworkFlushError() throws Exception {
        String uriString = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_WITH_FLUSH_ERROR_ID + URI_NETWORK_MODIF_PARAMS;

        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);

        assertThrows(PowsyblException.class.getName(),
                        NestedServletException.class, () -> mockMvc.perform(post(uriString).content(groovyScriptInfosJson)
                                        .contentType(MediaType.APPLICATION_JSON)));
        // apply groovy script with 2 modifications with network flush error
        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, true, false).size());
    }

    @Test
    public void testMultipleModificationsWithError() throws Exception {
        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);

        // apply groovy script without error
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // apply groovy script with error ont the second
        groovyScriptInfos.setScript("network.getGenerator('idGenerator').targetP=30\nnetwork.getGenerator('there is no generator').targetP=40\n");
        groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);
        MvcResult mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());

        // no modifications have been saved
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
    }

    @Test
    public void testCreateTwoWindingsTransformerInBusBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new 2wt in voltage level with bus/breaker topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .ratedS(200.)
                .build();
        String twoWindingsTransformerCreationInfosJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrTwoWindings = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrTwoWindings.get(0), createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        twoWindingsTransformerCreationInfos = new TwoWindingsTransformerCreationEntity(
                "id2wt1Edited",
                "2wtNameEdited",
                150.,
                250.,
                1005.,
                1015.,
                350.,
                450.,
                200.,
                "v12",
                "bus12",
                "v1",
                "bus1",
                50.,
                55.,
                "cn1",
                ConnectablePosition.Direction.TOP,
                "cn2",
                ConnectablePosition.Direction.BOTTOM,
                null,
                null,
                false,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                0,
                1
                )
                .toModificationInfos();
        twoWindingsTransformerCreationInfos.setUuid(bsmlrTwoWindings.get(0).getUuid());

        // Update 2wt creation
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationUpdate = new TwoWindingsTransformerCreationEntity(
                "id2wt1Edited",
                "2wtNameEdited",
                150.,
                250.,
                1005.,
                1015.,
                350.,
                450.,
                200.,
                "v12",
                "bus12",
                "v1",
                "bus1",
                50.,
                55.,
                "cn12",
                ConnectablePosition.Direction.TOP,
                "cn22",
                ConnectablePosition.Direction.BOTTOM,
                null,
                null,
                false,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                2,
                3
        ).toModificationInfos();
        String twoWindingsTransformerCreationUpdateJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlrTwoWindings.get(0).getUuid()).content(twoWindingsTransformerCreationUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlrTwoWindings.get(0).getUuid()))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        TwoWindingsTransformerCreationInfos bsmlrWindingsTransformer = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrWindingsTransformer, MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(twoWindingsTransformerCreationInfos));

        // create 2wt with errors
        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage())).andReturn();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateTwoWindingsTransformerInNodeBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new 2wt in voltage level with Node/breaker topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build();
        String twoWindingsTransformerCreationInfosJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrTwoWindingsTransformer = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrTwoWindingsTransformer.get(0), createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        assertNotNull(network.getTwoWindingsTransformer("id2wt1"));  // transformer was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create transformer on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the transformer cannot be created
        twoWindingsTransformerCreationInfos.setEquipmentId("id2wt3");
        twoWindingsTransformerCreationInfos.setEquipmentName("name2wt3");
        twoWindingsTransformerCreationInfosJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getTwoWindingsTransformer("id2wt3"));  // transformer was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    private List<TapChangerStepCreationInfos> getTapChangerSteps() {
        return List.of(
                TapChangerStepCreationInfos.builder()
                        .r(39.78473)
                        .x(39.784725)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build(),
                TapChangerStepCreationInfos.builder()
                        .r(39.78474)
                        .x(39.784726)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build(),
                TapChangerStepCreationInfos.builder()
                        .r(39.78475)
                        .x(39.784727)
                        .g(0.)
                        .b(0.)
                        .rho(1.)
                        .build()
        );
    }

    private void testCreateTwoWindingsTransformerInNodeBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        final String transformerId = twoWindingsTransformerCreationInfos.getEquipmentId();

        String twoWindingsTransformerCreationInfosJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrTwoWindingsTransformer = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrTwoWindingsTransformer.get(0), createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, transformerId, Set.of("s1")));

        assertNotNull(network.getTwoWindingsTransformer(transformerId));  // transformer was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateTwoWindingsTransformerWithRatioTapChangerInNodeBreaker() throws Exception {
        final String transformerId = "id2wt1WithRatioTapChanger";
        // create new 2wt in voltage level with Node/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationInfos ratioTapChangerCreationInfos = RatioTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("v1load")
                .regulatingTerminalType("LOAD")
                .loadTapChangingCapabilities(true)
                .targetV(220.)
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId(transformerId)
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos);
    }

    @Test
    public void testCreateTwoWindingsTransformerWithPhaseTapChangerInNodeBreaker() throws Exception {
        final String transformerId = "id2wt1WithPhaseTapChanger";
        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger
        PhaseTapChangerCreationInfos phaseTapChangerCreationInfos = PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("v1load")
                .regulatingTerminalType("LOAD")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId(transformerId)
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(phaseTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos);
    }

    @Test
    public void testCreateTwoWindingsTransformerInMixedTopology() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new 2wt in voltage level with mixed topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v3")
                .busOrBusbarSectionId2("bus3")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build();

        String twoWindingsTransformerCreationInfosJson = objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_FULL_MIXED_TOPO).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrTwoWindingsTransformerCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrTwoWindingsTransformerCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        testNetworkModificationsCount(TEST_NETWORK_MIXED_TOPOLOGY_ID, 1);
    }

    private List<ModificationInfos> createSomeSwitchModifications(UUID groupId, int number) throws Exception {
        List<Boolean> openStates = List.of(true, false);
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentId("v1b1")
                .build();

        for (int i = 0; i < number; i++) {
            switchStatusModificationInfos.setEquipmentAttributeValue(openStates.get(i % 2));
            String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
            mockMvc.perform(post(URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + "&groupUuid=" + groupId + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID())
                            .content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        }
        var modificationList = modificationRepository.getModifications(groupId, true, true);
        assertEquals(number, modificationList.size());
        return modificationList;
    }

    @Test
    public void testDuplicateModification() throws Exception {
        // create 3 modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 3);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Duplicate [0] and [1], and append them at the end of the group modification list.
        // Also try to duplicate 2 un-existing modifications, that should be returned as errors.
        List<UUID> duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        List<UUID> badModificationUuidList = List.of(UUID.randomUUID(), UUID.randomUUID());
        duplicateModificationUuidList.addAll(badModificationUuidList);

        MvcResult mvcResult = mockMvc.perform(
            put("/v1/groups/" + TEST_GROUP_ID + "?action=COPY"
                    + "&networkUuid=" + TEST_NETWORK_ID
                    + "&reportUuid=" + TEST_REPORT_ID
                    + "&reporterId=" + UUID.randomUUID()
                    + "&variantId=" + NetworkCreation.VARIANT_ID)
                .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        List<UUID> resultModificationUuidList = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(badModificationUuidList, resultModificationUuidList); // bad uuids are returned

        var newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 5 modifications: first 0-1-2 are still the same, last 3-4 are new (duplicates of 0-1)
        assertEquals(5, newModificationList.size());
        assertEquals(modificationUuidList, newModificationUuidList.subList(0, 3));
        // compare duplicates 0 and 3 (same data except uuid)
        var modification0 = modificationList.get(0);
        var newModification3 = newModificationList.get(3);
        modification0.setUuid(null);
        newModification3.setUuid(null);
        assertEquals(modification0.toString(), newModification3.toString());
        // compare duplicates 1 and 4 (same data except uuid)
        var modification1 = modificationList.get(1);
        var newModification4 = newModificationList.get(4);
        modification1.setUuid(null);
        newModification4.setUuid(null);
        assertEquals(modification1.toString(), newModification4.toString());

        // bad request error case: wrong action param
        mockMvc.perform(
                put("/v1/groups/" + TEST_GROUP_ID + "?action=XXXXXXX")
                    .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest());

        // create 1 modification in another group
        UUID otherGroupId = UUID.randomUUID();
        List<ModificationInfos> modificationListOtherGroup = createSomeSwitchModifications(otherGroupId, 1);
        List<UUID> modificationUuidListOtherGroup = modificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Duplicate the same modifications, and append them at the end of this new group modification list.
        duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        mvcResult = mockMvc.perform(
                put("/v1/groups/" + otherGroupId + "?action=COPY"
                        + "&networkUuid=" + TEST_NETWORK_ID
                        + "&reportUuid=" + TEST_REPORT_ID
                        + "&reporterId=" + UUID.randomUUID()
                        + "&variantId=" + NetworkCreation.VARIANT_ID)
                    .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultModificationUuidList = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(List.of(), resultModificationUuidList); // no bad id => no error this time

        var newModificationListOtherGroup = modificationRepository.getModifications(otherGroupId, true, true);
        List<UUID> newModificationUuidListOtherGroup = newModificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 3 modifications in new group: first 0 is still the same, last 1-2 are new (duplicates of 0-1 from first group)
        assertEquals(3, newModificationListOtherGroup.size());
        assertEquals(modificationUuidListOtherGroup, newModificationUuidListOtherGroup.subList(0, 1));
        // compare duplicates
        var newModification1 = newModificationListOtherGroup.get(1);
        newModification1.setUuid(null);
        assertEquals(modification0.toString(), newModification1.toString());
        var newModification2 = newModificationListOtherGroup.get(2);
        newModification2.setUuid(null);
        assertEquals(modification1.toString(), newModification2.toString());
    }

    @Test
    public void testDuplicateModificationWithUnexistingId() throws Exception {
        // create 1 modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Try to copy an unexisting Modification
        List<UUID> duplicateModificationUuidList = List.of(UUID.randomUUID());
        MvcResult mvcResult = mockMvc.perform(
                        put("/v1/groups/" + TEST_GROUP_ID + "?action=COPY"
                                + "&networkUuid=" + TEST_NETWORK_ID
                                + "&reportUuid=" + TEST_REPORT_ID
                                + "&reporterId=" + UUID.randomUUID()
                                + "&variantId=" + NetworkCreation.VARIANT_ID)
                                .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        List<UUID> resultModificationUuidList = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(duplicateModificationUuidList, resultModificationUuidList); // bad uuids are returned

        var newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // we still have the same and only modification
        assertEquals(newModificationUuidList, modificationUuidList);
    }

    private void testMoveModification(UUID originGroupUuid, Boolean canBuild) throws Exception {
        // create 2 modifications
        List<UUID> modificationUuidList = createSomeSwitchModifications(TEST_GROUP_ID, 2).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // swap modifications: move [1] before [0]
        List<UUID> movingModificationUuidList = Collections.singletonList(modificationUuidList.get(1));
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=MOVE"
                + "&networkUuid=" + TEST_NETWORK_ID
                + "&reportUuid=" + TEST_REPORT_ID
                + "&reporterId=" + UUID.randomUUID()
                + "&variantId=" + NetworkCreation.VARIANT_ID
                + "&before=" + modificationUuidList.get(0);
        if (originGroupUuid != null) {
            url = url + "&originGroupUuid=" + TEST_GROUP_ID;
        }
        if (canBuild != null) {
            url = url + "&buid=" + canBuild;
        }
        mockMvc.perform(put(url).content(objectWriter.writeValueAsString(movingModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var newModificationUuidList = modificationRepository.getModifications(TEST_GROUP_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newModificationUuidList);
        Collections.reverse(newModificationUuidList);

        assertEquals(modificationUuidList, newModificationUuidList);
    }

    @Test
    public void testMoveModificationWithOrigin() throws Exception {
        testMoveModification(TEST_GROUP_ID, Boolean.TRUE);
    }

    @Test
    public void testMoveModificationWithoutOrigin() throws Exception {
        testMoveModification(null, null);
    }

    @Test
    public void testMoveModificationWithUnexistingId() throws Exception {
        // create 2 modifications
        List<UUID> modificationUuidList = createSomeSwitchModifications(TEST_GROUP_ID, 2).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // try to move an unexisting modification before [0]: no error, no change
        List<UUID> movingModificationUuidList = List.of(UUID.randomUUID());
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=MOVE"
                + "&networkUuid=" + TEST_NETWORK_ID
                + "&reportUuid=" + TEST_REPORT_ID
                + "&reporterId=" + UUID.randomUUID()
                + "&variantId=" + NetworkCreation.VARIANT_ID
                + "&before=" + modificationUuidList.get(0)
                + "&originGroupUuid=" + TEST_GROUP_ID;
        mockMvc.perform(put(url).content(objectWriter.writeValueAsString(movingModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var newModificationUuidList = modificationRepository.getModifications(TEST_GROUP_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newModificationUuidList);
        // nothing has changed in modification group
        assertEquals(modificationUuidList, newModificationUuidList);
    }

    @Test
    public void testDuplicateModificationGroup() throws Exception {

        VoltageLevelCreationInfos vl1 = ModificationCreation.getCreationVoltageLevel("s1", "vl1Id", "vl1Name");
        mockMvc.perform(
                post(URI_NETWORK_MODIF_BUS_BREAKER)
                    .content(objectWriter.writeValueAsString(vl1))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn().getResponse().getContentAsString();

        VoltageLevelCreationInfos vl2 = ModificationCreation.getCreationVoltageLevel("s1", "vl2Id", "vl2Name");
        mockMvc.perform(
                post(URI_NETWORK_MODIF_BUS_BREAKER)
                    .content(objectWriter.writeValueAsString(vl2))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn().getResponse().getContentAsString();

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
        CurrentLimitsInfos c1 = new CurrentLimitsInfos();
        c1.setPermanentLimit(100.0);
        CurrentLimitsInfos c2 = new CurrentLimitsInfos();
        c2.setPermanentLimit(200.0);
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits1(c1)
                .currentLimits2(c2)
                .build();

        String resultAsString = mockMvc.perform(
            post(URI_NETWORK_MODIF_BUS_BREAKER)
                .content(objectWriter.writeValueAsString(lineCreationInfos))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn().getResponse().getContentAsString();

        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modifications.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        // create new 2wt in voltage level with bus/breaker topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .type(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION)
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .currentLimits1(c1)
                .currentLimits2(c2)
                .build();

        mockMvc.perform(
                post(URI_NETWORK_MODIF_BUS_BREAKER)
                    .content(objectWriter.writeValueAsString(twoWindingsTransformerCreationInfos))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        //create a lineAttached
        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId("attachmentLine")
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();

        LineAttachToVoltageLevelInfos lineAttachToVL = new LineAttachToVoltageLevelInfos("line3",
                10.0, "AttPointId", "attPointName", null, "v4",
                "1.A", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");
        lineAttachToVL.setType(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);

        mockMvc.perform(
                post(URI_NETWORK_MODIF)
                    .content(objectWriter.writeValueAsString(lineAttachToVL))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        //create a lineSplit
        LineSplitWithVoltageLevelInfos lineSplitWoVL = new LineSplitWithVoltageLevelInfos("line3", 10.0, null, "v4", "1.A",
                "nl1", "NewLine1", "nl2", "NewLine2");
        lineSplitWoVL.setType(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL);

        mockMvc.perform(
                post(URI_NETWORK_MODIF)
                    .content(objectWriter.writeValueAsString(lineSplitWoVL))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        //create a generator
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v2", "idGenerator1", "nameGenerator1", "1B", "v2load", "LOAD", "v1");
        generatorCreationInfos.setType(ModificationType.GENERATOR_CREATION);
        mockMvc.perform(
                post(URI_NETWORK_MODIF)
                    .content(objectWriter.writeValueAsString(generatorCreationInfos))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 7);

        //test copy group
        UUID newGroupUuid = UUID.randomUUID();
        String uriStringGroups = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(uriStringGroups)).andExpect(status().isOk());

        testNetworkModificationsCount(newGroupUuid, 7);
    }

    @Test
    public void replaceTeePointByVoltageLevelOnLineDuplicateModificationGroupTest() throws Exception {
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = new LinesAttachToSplitLinesInfos("l1", "l2", "l3", "v4", "bbs2", "nl1", "NewLine1", "nl2", "NewLine2");
        linesAttachToSplitLinesInfos.setType(ModificationType.LINES_ATTACH_TO_SPLIT_LINES);

        mockMvc.perform(post(URI_NETWORK_WITH_TEE_POINT_MODIF)
                                .content(objectWriter.writeValueAsString(linesAttachToSplitLinesInfos))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // test copy group
        UUID newGroupUuid = UUID.randomUUID();
        String copyGroupUriString = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(copyGroupUriString))
                .andExpect(status().isOk());

        testNetworkModificationsCount(newGroupUuid, 1);
    }

    @Test
    public void testGroupDuplication() throws Exception {
        // create new load in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
                .type(ModificationType.LOAD_CREATION)
                .equipmentId("idLoad1")
                .equipmentName("nameLoad1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .loadType(LoadType.AUXILIARY)
                .activePower(100.0)
                .reactivePower(60.0)
                .connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionName("bottom")
                .build();
        String loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
        assertNotNull(network.getLoad("idLoad1"));  // load was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        UUID duplicatedGroupUuid = UUID.randomUUID();
        String uriStringGroups = "/v1/groups?duplicateFrom=" + TEST_GROUP_ID + "&groupUuid=" + duplicatedGroupUuid + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
        mockMvc.perform(post(uriStringGroups)).andExpect(status().isOk());
        testNetworkModificationsCount(duplicatedGroupUuid, 1);

        uriStringGroups = "/v1/groups?duplicateFrom=" + UUID.randomUUID() + "&groupUuid=" + UUID.randomUUID() + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
        mockMvc.perform(post(uriStringGroups).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
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

    @Test
    public void shouldGetPosition() {
        var network = networkStoreService.getNetwork(TEST_NETWORK_ID);
        var network2 = networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID);
        var vl = network.getVoltageLevel("v2");
        var vl2 = network2.getVoltageLevel("v2");
        assertEquals(9, vl.getConnectableCount());
        assertEquals(0, vl2.getConnectableCount());
        assertNotNull(network.getBusbarSection("1B"));
        assertNotNull(network.getBusbarSection("1.1"));

        var result = ModificationUtils.getInstance().getPosition("1B", network, vl);
        var result2 = ModificationUtils.getInstance().getPosition("1.1", network2, vl2);
        assertEquals(6, result);
        assertEquals(0, result2);

        ModificationUtils modificationUtils = ModificationUtils.getInstance();
        assertThrows(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "Bus bar section invalidBbsId not found").getMessage(),
                NetworkModificationException.class, () -> modificationUtils.getPosition("invalidBbsId", network, vl)
        );
    }

    @Test
    public void testGetPositionAfterAndBefore() {
        Network testNetwork = new NetworkFactoryImpl().createNetwork("testNetwork", "test");
        Substation s1 = testNetwork.newSubstation().setId("s1").setName("s1").setCountry(Country.FR).add();
        VoltageLevel v1 = s1.newVoltageLevel().setId("v1").setName("v1").setTopologyKind(TopologyKind.NODE_BREAKER).setNominalV(380.).add();
        BusbarSection bbs1 = v1.getNodeBreakerView().newBusbarSection().setId("bbs1").setName("bbs1").setNode(0).add();
        bbs1.newExtension(BusbarSectionPositionAdder.class).withBusbarIndex(1).withSectionIndex(1).add();
        BusbarSection bbs2 = v1.getNodeBreakerView().newBusbarSection().setId("bbs2").setName("bbs2").setNode(1).add();
        bbs2.newExtension(BusbarSectionPositionAdder.class).withBusbarIndex(1).withSectionIndex(2).add();

        Load load1 = v1.newLoad().setId("load1").setName("load1").setNode(2).setP0(1.).setQ0(1.).add();
        load1.newExtension(ConnectablePositionAdder.class).newFeeder().withName("load1").withOrder(1).withDirection(ConnectablePosition.Direction.TOP).add().add();
        v1.getNodeBreakerView().newSwitch().setId("switch1").setName("switch1").setKind(SwitchKind.BREAKER).setRetained(false).setOpen(false).setFictitious(false).setNode1(0).setNode2(2).add();
        Load load2 = v1.newLoad().setId("load2").setName("load2").setNode(3).setP0(1.).setQ0(1.).add();
        load2.newExtension(ConnectablePositionAdder.class).newFeeder().withName("load2").withOrder(2).withDirection(ConnectablePosition.Direction.TOP).add().add();
        v1.getNodeBreakerView().newSwitch().setId("switch2").setName("switch2").setKind(SwitchKind.BREAKER).setRetained(false).setOpen(false).setFictitious(false).setNode1(1).setNode2(3).add();

        assertEquals(0, ModificationUtils.getInstance().getPosition("bbs1", testNetwork, v1));
    }
}
