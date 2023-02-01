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

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos;
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
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_BUS_BREAKER = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_BUS_BREAKER_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_BAD_NETWORK = URI_NETWORK_MODIF_BASE + "?networkUuid=" + NOT_FOUND_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_BAD_VARIANT = URI_NETWORK_MODIF + "&variantId=" + VARIANT_NOT_EXISTING_ID;
    private static final String URI_NETWORK_MODIF_2 = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID_2 + URI_NETWORK_MODIF_PARAMS;

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
        String errorMessage = assertThrows(NetworkModificationException.class, () -> networkModificationService.updateNetworkModification(modificationUuid, modificationInfos)).getMessage();
        assertEquals(new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)).getMessage(), errorMessage);
        assertThrows(NullPointerException.class, () -> networkModificationService.updateNetworkModification(modificationUuid, null));
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
    public void testNetworkModificationsWithErrorOnNetworkFlush() throws Exception {
        String uriString = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_WITH_FLUSH_ERROR_ID + URI_NETWORK_MODIF_PARAMS;

        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);

        mockMvc.perform(post(uriString).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isBadRequest());

        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, false).size());
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

        assertEquals(2, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
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

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

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

        testNetworkModificationsCount(TEST_GROUP_ID, 6);

        //test copy group
        UUID newGroupUuid = UUID.randomUUID();
        String uriStringGroups = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(uriStringGroups)).andExpect(status().isOk());

        testNetworkModificationsCount(newGroupUuid, 6);
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

    @Test
    public void testTombstonedEquipmentInfos() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());
        assertTrue(equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v1load")
                .build();
        String equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);

        // delete load
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> bsmlrEquipmentDeletion = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrEquipmentDeletion.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1load", "LOAD", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // load and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getLoad("v1load"));
        assertNull(network.getSwitch("v1d1"));
        assertNull(network.getSwitch("v1b1"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1load", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1d1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1b1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // Test delete load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be deleted
        equipmentDeletionInfos.setEquipmentType("LOAD");
        equipmentDeletionInfos.setEquipmentId("v3load");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletions = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(deletions.isEmpty());  // no modifications returned
        assertNotNull(network.getLoad("v3load"));  // load was not deleted
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // delete shunt compensator
        equipmentDeletionInfos.setEquipmentType("SHUNT_COMPENSATOR");
        equipmentDeletionInfos.setEquipmentId("v2shunt");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsEquipmentDeletion = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsEquipmentDeletion.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2shunt", "SHUNT_COMPENSATOR", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        // shunt compensator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getShuntCompensator("v2shunt"));
        assertNull(network.getSwitch("v2bshunt"));
        assertNull(network.getSwitch("v2dshunt"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2shunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bshunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dshunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete generator
        equipmentDeletionInfos.setEquipmentType("GENERATOR");
        equipmentDeletionInfos.setEquipmentId("idGenerator");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDeletionNetwork = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDeletionNetwork.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "idGenerator", "GENERATOR", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        // generator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getGenerator("idGenerator"));
        assertNull(network.getSwitch("v2bgenerator"));
        assertNull(network.getSwitch("v2dgenerator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("idGenerator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bgenerator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dgenerator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete line
        equipmentDeletionInfos.setEquipmentType("LINE");
        equipmentDeletionInfos.setEquipmentId("line2");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
             .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDeletionEquipement = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDeletionEquipement.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "line2", "LINE", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        // line and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getLine("line2"));
        assertNull(network.getSwitch("v1dl2"));
        assertNull(network.getSwitch("v1bl2"));
        assertNull(network.getSwitch("v3dl2"));
        assertNull(network.getSwitch("v3bl2"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("line2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1bl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete two windings transformer
        equipmentDeletionInfos.setEquipmentType("TWO_WINDINGS_TRANSFORMER");
        equipmentDeletionInfos.setEquipmentId("trf1");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDelEquipement = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDelEquipement.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf1", "TWO_WINDINGS_TRANSFORMER", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 6);

        // 2 windings transformer and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getTwoWindingsTransformer("trf1"));
        assertNull(network.getSwitch("v1btrf1"));
        // disconnector 'v1dtrf1' was not removed (2wt 'trf1' in double feeder with 3wt 'trf6' in voltage level 'v1')
        assertNull(network.getSwitch("v1dtrf1"));
        assertNull(network.getSwitch("v2btrf1"));
        assertNull(network.getSwitch("v2dtrf1"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("trf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1btrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dtrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2btrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dtrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete three windings transformer
        equipmentDeletionInfos.setEquipmentType("THREE_WINDINGS_TRANSFORMER");
        equipmentDeletionInfos.setEquipmentId("trf6");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDelEquip = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDelEquip.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf6", "THREE_WINDINGS_TRANSFORMER", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 7);

        // 3 windings transformer and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getThreeWindingsTransformer("trf6"));
        // breaker 'v1btrf6' was not removed (special connection of 3wt 'trf6' in voltage level 'v1')
        // disconnector 'v1dtrf6' was not removed (special connection of 3wt 'trf6' in voltage level 'v1')
        assertNotNull(network.getSwitch("v1btrf6"));
        assertNotNull(network.getSwitch("v1dtrf6"));
        assertNull(network.getSwitch("v2btrf6"));
        assertNull(network.getSwitch("v2dtrf6"));
        assertNull(network.getSwitch("v4btrf6"));
        assertNull(network.getSwitch("v4dtrf6"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("trf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1btrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dtrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2btrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dtrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v4btrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v4dtrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete static var compensator
        equipmentDeletionInfos.setEquipmentType("STATIC_VAR_COMPENSATOR");
        equipmentDeletionInfos.setEquipmentId("v3Compensator");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsEquipement = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsEquipement.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Compensator", "STATIC_VAR_COMPENSATOR", Set.of("s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 8);

        // static var compensator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getStaticVarCompensator("v3Compensator"));
        assertNull(network.getSwitch("v3dCompensator"));
        assertNull(network.getSwitch("v3bCompensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3Compensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dCompensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bCompensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete battery
        equipmentDeletionInfos.setEquipmentType("BATTERY");
        equipmentDeletionInfos.setEquipmentId("v3Battery");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV3Battery = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV3Battery.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Battery", "BATTERY", Set.of("s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 9);

        // battery and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getBattery("v3Battery"));
        assertNull(network.getSwitch("v3dBattery"));
        assertNull(network.getSwitch("v3bBattery"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3Battery", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dBattery", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bBattery", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete dangling line
        equipmentDeletionInfos.setEquipmentType("DANGLING_LINE");
        equipmentDeletionInfos.setEquipmentId("v2Dangling");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV2Dangling = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV2Dangling.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2Dangling", "DANGLING_LINE", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 10);

        // dangling line and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getDanglingLine("v2Dangling"));
        assertNull(network.getSwitch("v2bdangling"));
        assertNull(network.getSwitch("v2ddangling"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2Dangling", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bdangling", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2ddangling", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete hvdc line
        equipmentDeletionInfos.setEquipmentType("HVDC_LINE");
        equipmentDeletionInfos.setEquipmentId("hvdcLine");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionshHdcLine = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionshHdcLine.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "hvdcLine", "HVDC_LINE", Set.of("s1")));
        testNetworkModificationsCount(TEST_GROUP_ID, 11);

        // hvdc line has been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getHvdcLine("hvdcLine"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("hvdcLine", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete vsc converter station
        equipmentDeletionInfos.setEquipmentType("HVDC_CONVERTER_STATION");
        equipmentDeletionInfos.setEquipmentId("v2vsc");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_2).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV2Vsc = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV2Vsc.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2vsc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 12);

        // vsc converter station and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network2.getVscConverterStation("v2vsc"));
        assertNull(network2.getSwitch("v2bvsc"));
        assertNull(network2.getSwitch("v2dvsc"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2vsc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bvsc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dvsc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete lcc converter station
        equipmentDeletionInfos.setEquipmentType("HVDC_CONVERTER_STATION");
        equipmentDeletionInfos.setEquipmentId("v1lcc");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_2).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV1Lcc = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV1Lcc.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1lcc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 13);

        // lcc converter station and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network2.getLccConverterStation("v1lcc"));
        assertNull(network2.getSwitch("v1dlcc"));
        assertNull(network2.getSwitch("v1blcc"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1lcc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dlcc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1blcc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete voltage level
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v5");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        List<EquipmentDeletionInfos> deletionsV5 = modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(EquipmentDeletionInfos.class::cast).collect(Collectors.toList());
        EquipmentDeletionInfos lastCreatedEquipmentDeletion = deletionsV5.get(deletionsV5.size() - 1);
//        assertThat(deletionsV5.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v5", "VOLTAGE_LEVEL", Set.of("s3")));
        assertEquals(ModificationType.EQUIPMENT_DELETION, lastCreatedEquipmentDeletion.getType());
        assertEquals("VOLTAGE_LEVEL", lastCreatedEquipmentDeletion.getEquipmentType());
        assertEquals("v5", lastCreatedEquipmentDeletion.getEquipmentId());

        testNetworkModificationsCount(TEST_GROUP_ID, 14);

        // voltage level and equipments have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getVoltageLevel("v5"));
        assertNull(network.getBusbarSection("1A1"));
        assertNull(network.getLoad("v5load"));
        assertNull(network.getGenerator("v5generator"));
        assertNull(network.getShuntCompensator("v5shunt"));
        assertNull(network.getStaticVarCompensator("v5Compensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("1A1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5load", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5generator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5shunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5Compensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // try to delete voltage level (Internal error because the vl is still connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        mockMvc.perform(post(URI_NETWORK_MODIF).content(objectWriter.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is5xxServerError(),
                content().string(new NetworkModificationException(DELETE_EQUIPMENT_ERROR,
                    new PowsyblException(new AssertionError("The voltage level 'v4' cannot be removed because of a remaining THREE_WINDINGS_TRANSFORMER"))).getMessage()));
        assertNotNull(network.getVoltageLevel("v4"));

        // delete substation
        equipmentDeletionInfos.setEquipmentType("SUBSTATION");
        equipmentDeletionInfos.setEquipmentId("s3");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
//        resultAsString = mvcResult.getResponse().getContentAsString();
//        List<EquipmentDeletionInfos> deletionsS3 = mapper.readValue(resultAsString, new TypeReference<>() { });
//        assertThat(deletionsS3.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "s3", "SUBSTATION", Set.of("s3")));
        List<EquipmentDeletionInfos> deletionsS3 = modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(EquipmentDeletionInfos.class::cast).collect(Collectors.toList());
        lastCreatedEquipmentDeletion = deletionsS3.get(deletionsS3.size() - 1);
        assertEquals(ModificationType.EQUIPMENT_DELETION, lastCreatedEquipmentDeletion.getType());
        assertEquals("SUBSTATION", lastCreatedEquipmentDeletion.getEquipmentType());
        assertEquals("s3", lastCreatedEquipmentDeletion.getEquipmentId());

        testNetworkModificationsCount(TEST_GROUP_ID, 16);

        // substation and equipments have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getSubstation("s3"));
        assertNull(network.getVoltageLevel("v6"));
        assertNull(network.getBusbarSection("1B1"));
        assertNull(network.getLoad("v6load"));
        assertNull(network.getGenerator("v6generator"));
        assertNull(network.getShuntCompensator("v6shunt"));
        assertNull(network.getStaticVarCompensator("v6Compensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("s3", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("1B1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6load", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6generator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6shunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6Compensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // try to delete substation (Internal error because the substation is still connected)
        equipmentDeletionInfos.setEquipmentType("SUBSTATION");
        equipmentDeletionInfos.setEquipmentId("s2");
        mockMvc.perform(post(URI_NETWORK_MODIF).content(objectWriter.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is5xxServerError(),
                content().string(new NetworkModificationException(DELETE_EQUIPMENT_ERROR,
                    "The substation s2 is still connected to another substation").getMessage()));
        assertNotNull(network.getSubstation("s2"));

        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());
        assertEquals(55, equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).size());
        assertEquals(6, equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID_2).size());
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
        String errorMessage = assertThrows(NetworkModificationException.class, () -> modificationUtils.getPosition("invalidBbsId", network, vl)).getMessage();
        assertEquals(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "invalidBbsId").getMessage(), errorMessage);
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
