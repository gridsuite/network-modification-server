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
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.ConnectablePositionAdder;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import lombok.SneakyThrows;
import nl.jqno.equalsverifier.EqualsVerifier;

import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.Impacts.TestImpactUtils;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.LoadCreationInfos.LoadCreationInfosBuilder;
import org.gridsuite.modification.server.dto.catalog.AerialLineTypeInfos;
import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;
import org.gridsuite.modification.server.dto.catalog.UndergroundLineTypeInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.TombstonedEquipmentInfosRepository;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.Impacts.TestImpactUtils.*;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.argThat;
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
@SpringBootTest
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

    private static final String URI_NETWORK_WITH_TEE_POINT_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_WITH_TEE_POINT_ID + URI_NETWORK_MODIF_PARAMS;

    private static final String URI_LINE_CATALOG = URI_NETWORK_MODIF_BASE + "/catalog/line_types";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_1 = "/line_types_catalog_1.json";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_2 = "/line_types_catalog_2.json";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper mapper;
    @MockBean
    private NetworkStoreService networkStoreService;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @MockBean
    private ReportService reportService;

    @Autowired
    private NetworkModificationService networkModificationService;

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Autowired
    private EquipmentInfosRepository equipmentInfosRepository;

    @Autowired
    private TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    private ObjectWriter objectWriter;
    private Network network;

    private Network network2;

    private Network networkWithTeePoint;
    private Network networkBusBreaker;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> network);
        network2 = NetworkCreation.create(TEST_NETWORK_ID_2, false);
        when(networkStoreService.getNetwork(TEST_NETWORK_ID_2)).then((Answer<Network>) invocation -> network2);

        networkWithTeePoint = NetworkWithTeePoint.create(TEST_NETWORK_WITH_TEE_POINT_ID);
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_TEE_POINT_ID)).then((Answer<Network>) invocation -> networkWithTeePoint);

        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_FLUSH_ERROR_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_WITH_FLUSH_ERROR_ID, true));

        networkBusBreaker = NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID);
        when(networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID)).then((Answer<Network>) invocation -> networkBusBreaker);

        when(networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID)).then((Answer<Network>) invocation -> NetworkCreation.createMixedTopology(TEST_NETWORK_MIXED_TOPOLOGY_ID));

        doThrow(new PowsyblException()).when(networkStoreService).flush(argThat(n -> TEST_NETWORK_WITH_FLUSH_ERROR_ID.toString().equals(n.getId())));

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

    private boolean existTombstonedEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantId).stream().anyMatch(t -> t.getId().equals(equipmentId));
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
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().equipmentId("idLoad").build();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationInfos.setEquipmentId(null)).getMessage());
    }

    @SneakyThrows
    @Test
    public void testNetworkNotFound() {
        mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK)
            .content(objectWriter.writeValueAsString(LoadCreationInfos.builder().equipmentId("id").build()))
            .contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage())
            );
    }

    @Test
    public void assertThrowsUpdateModificationNotFound() {
        UUID modificationUuid = UUID.randomUUID();
        ModificationInfos modificationInfos = LoadCreationInfos.builder().equipmentId("id").build();
        String errorMessage = assertThrows(NetworkModificationException.class, () -> networkModificationService.updateNetworkModification(modificationUuid, modificationInfos)).getMessage();
        assertEquals(new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)).getMessage(), errorMessage);
        assertThrows(NullPointerException.class, () -> networkModificationService.updateNetworkModification(modificationUuid, null));
    }

    @Test
    public void testModificationGroups() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
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
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, "v1b1", Set.of("s1"));

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
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);

        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, "v1b1", Set.of("s1"));

        List<ModificationInfos> modifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
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
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);

        mockMvc.perform(post(uriString).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().is5xxServerError());

        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, false).size());
    }

    @Test
    public void testMultipleModificationsWithError() throws Exception {
        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);

        // apply groovy script without error
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // apply groovy script with error on the second
        groovyScriptInfos.setScript("network.getGenerator('there is no generator').targetP=30\nnetwork.getGenerator('idGenerator').targetP=40\n");
        groovyScriptInfosJson = objectWriter.writeValueAsString(groovyScriptInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(network.getGenerator("idGenerator"));
        assertEquals(20, network.getGenerator("idGenerator").getTargetP(), 0.1);
        assertLogMessage("Technical error: java.lang.NullPointerException: Cannot set property 'targetP' on null object",
                groovyScriptInfos.getErrorType().name(), reportService);

        assertEquals(2, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
    }

    private List<ModificationInfos> createSomeSwitchModifications(UUID groupId, int number) throws Exception {
        List<Boolean> openStates = List.of(true, false);
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
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
        List<UUID> duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        List<UUID> badModificationUuidList = List.of(UUID.randomUUID(), UUID.randomUUID());
        duplicateModificationUuidList.addAll(badModificationUuidList);

        mockMvc.perform(
            put("/v1/groups/" + TEST_GROUP_ID + "?action=COPY"
                    + "&networkUuid=" + TEST_NETWORK_ID
                    + "&reportUuid=" + TEST_REPORT_ID
                    + "&reporterId=" + UUID.randomUUID()
                    + "&variantId=" + NetworkCreation.VARIANT_ID)
                .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        var newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 5 modifications: first 0-1-2 are still the same, last 3-4 are new (duplicates of 0-1)
        assertEquals(5, newModificationList.size());
        assertEquals(modificationUuidList, newModificationUuidList.subList(0, 3));
        // compare duplicates 0 and 3 (same data except uuid)
        assertThat(newModificationList.get(3), MatcherModificationInfos.createMatcherModificationInfos(modificationList.get(0)));

        // compare duplicates 1 and 4 (same data except uuid)
        assertThat(newModificationList.get(4), MatcherModificationInfos.createMatcherModificationInfos(modificationList.get(1)));

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
        mockMvc.perform(
                put("/v1/groups/" + otherGroupId + "?action=COPY"
                        + "&networkUuid=" + TEST_NETWORK_ID
                        + "&reportUuid=" + TEST_REPORT_ID
                        + "&reporterId=" + UUID.randomUUID()
                        + "&variantId=" + NetworkCreation.VARIANT_ID)
                    .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        var newModificationListOtherGroup = modificationRepository.getModifications(otherGroupId, true, true);
        List<UUID> newModificationUuidListOtherGroup = newModificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 3 modifications in new group: first 0 is still the same, last 1-2 are new (duplicates of 0-1 from first group)
        assertEquals(3, newModificationListOtherGroup.size());
        assertEquals(modificationUuidListOtherGroup, newModificationUuidListOtherGroup.subList(0, 1));
        // compare duplicates
        assertThat(newModificationListOtherGroup.get(1), MatcherModificationInfos.createMatcherModificationInfos(modificationList.get(0)));
        assertThat(newModificationListOtherGroup.get(2), MatcherModificationInfos.createMatcherModificationInfos(modificationList.get(1)));
    }

    @Test
    public void testDuplicateModificationWithUnexistingId() throws Exception {
        // create 1 modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Try to copy an unexisting Modification
        List<UUID> duplicateModificationUuidList = List.of(UUID.randomUUID());
        mockMvc.perform(
                put("/v1/groups/" + TEST_GROUP_ID + "?action=COPY"
                        + "&networkUuid=" + TEST_NETWORK_ID
                        + "&reportUuid=" + TEST_REPORT_ID
                        + "&reporterId=" + UUID.randomUUID()
                        + "&variantId=" + NetworkCreation.VARIANT_ID)
                        .content(objectWriter.writeValueAsString(duplicateModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

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

    @SneakyThrows
    @Test
    public void createGeneratorWithStartup() {

        // create and build generator without startup
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v2", "idGenerator1", "nameGenerator1", "1B", "v2load", "LOAD", "v1");
        String generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);

        mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        GeneratorStartup generatorStartup = network.getGenerator("idGenerator1").getExtension(GeneratorStartup.class);
        assertNull(generatorStartup);

        // same for bus breaker
        GeneratorCreationInfos generatorCreationInfosBusBreaker = ModificationCreation.getCreationGenerator("v1", "idGenerator2", "nameGenerator2", "bus1", "idGenerator1", "GENERATOR", "v1");
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfosBusBreaker);

        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        generatorStartup = networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID).getGenerator("idGenerator2").getExtension(GeneratorStartup.class);
        assertNull(generatorStartup);

        // create and build generator with startup
        generatorCreationInfos.setEquipmentId("idGenerator21");
        generatorCreationInfos.setMarginalCost(8.);
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);

        mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        generatorStartup = network.getGenerator("idGenerator21").getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup);
        assertEquals(Double.NaN, generatorStartup.getPlannedActivePowerSetpoint(), 0);
        assertEquals(8., generatorStartup.getMarginalCost(), 0);
        assertEquals(Double.NaN, generatorStartup.getPlannedOutageRate(), 0);
        assertEquals(Double.NaN, generatorStartup.getForcedOutageRate(), 0);

        // same for bus breaker
        generatorCreationInfosBusBreaker.setEquipmentId("idGenerator3");
        generatorCreationInfosBusBreaker.setPlannedOutageRate(80.);
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfosBusBreaker);

        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        generatorStartup = networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID).getGenerator("idGenerator3").getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup);
        assertEquals(Double.NaN, generatorStartup.getPlannedActivePowerSetpoint(), 0);
        assertEquals(Double.NaN, generatorStartup.getMarginalCost(), 0);
        assertEquals(80., generatorStartup.getPlannedOutageRate(), 0);
        assertEquals(Double.NaN, generatorStartup.getForcedOutageRate(), 0);
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

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
        CurrentLimitsInfos c1 = new CurrentLimitsInfos();
        c1.setPermanentLimit(100.0);
        CurrentLimitsInfos c2 = new CurrentLimitsInfos();
        c2.setPermanentLimit(200.0);
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
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
        testBranchCreationImpacts(mapper, resultAsString, IdentifiableType.LINE, "idLine1", Set.of("s1", "s2"));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        //create a lineAttached
        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .equipmentId("attachmentLine")
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();

        LineAttachToVoltageLevelInfos lineAttachToVL = new LineAttachToVoltageLevelInfos("line3",
                10.0, "AttPointId", "attPointName", null, "v4",
                "1.A", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");

        mockMvc.perform(
                post(URI_NETWORK_MODIF)
                    .content(objectWriter.writeValueAsString(lineAttachToVL))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        //create a lineSplit
        LineSplitWithVoltageLevelInfos lineSplitWoVL = new LineSplitWithVoltageLevelInfos("line1", 10.0, null, "v4", "1.A",
                "nl11", "NewLine11", "nl12", "NewLine12");

        mockMvc.perform(
                post(URI_NETWORK_MODIF)
                    .content(objectWriter.writeValueAsString(lineSplitWoVL))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        //create a generator
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v2", "idGenerator1", "nameGenerator1", "1B", "v2load", "LOAD", "v1");
        mockMvc.perform(
                post(URI_NETWORK_MODIF)
                    .content(objectWriter.writeValueAsString(generatorCreationInfos))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        //test copy group
        UUID newGroupUuid = UUID.randomUUID();
        String uriStringGroups = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(uriStringGroups)).andExpect(status().isOk());

        testNetworkModificationsCount(newGroupUuid, 5);
    }

    @Test
    public void replaceTeePointByVoltageLevelOnLineDuplicateModificationGroupTest() throws Exception {
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = new LinesAttachToSplitLinesInfos("l1", "l2", "l3", "v4", "bbs2", "nl1", "NewLine1", "nl2", "NewLine2");

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
    @SneakyThrows
    public void testTombstonedEquipmentInfos() {
        MvcResult mvcResult;

        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).isEmpty());
        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID).isEmpty());
        assertTrue(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).isEmpty());
        assertTrue(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID).isEmpty());

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType("LOAD")
                .equipmentId("v1load")
                .build();
        String equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);

        // delete load
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.LOAD, "v1load", "v1b1", "v1d1", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test delete load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be deleted
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.LOAD.name());
        equipmentDeletionInfos.setEquipmentId("v3load");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationResult.isEmpty());  // no modification apply
        assertNotNull(network.getLoad("v3load"));  // load was not deleted
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // delete shunt compensator
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.SHUNT_COMPENSATOR.name());
        equipmentDeletionInfos.setEquipmentId("v2shunt");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.SHUNT_COMPENSATOR, "v2shunt", "v2bshunt", "v2dshunt", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        // delete generator
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.GENERATOR.name());
        equipmentDeletionInfos.setEquipmentId("idGenerator");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.GENERATOR, "idGenerator", "v2bgenerator", "v2dgenerator", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        // delete line
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.LINE.name());
        equipmentDeletionInfos.setEquipmentId("line2");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
             .andExpect(status().isOk()).andReturn();
        testBranchDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.LINE, "line2", "v1bl2", "v1dl2", "s1", "v3bl2", "v3dl2", "s2");
        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        // delete two windings transformer
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.TWO_WINDINGS_TRANSFORMER.name());
        equipmentDeletionInfos.setEquipmentId("trf1");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testBranchDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.TWO_WINDINGS_TRANSFORMER, "trf1", "v1btrf1", "v1dtrf1", "s1", "v2btrf1", "v2dtrf1", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 6);

        // delete three windings transformer
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.THREE_WINDINGS_TRANSFORMER.name());
        equipmentDeletionInfos.setEquipmentId("trf6");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        test3WTDeletionImpacts(mvcResult.getResponse().getContentAsString(), "trf6", "v1btrf6", "v1dtrf6", "v2btrf6", "v2dtrf6", "v4btrf6", "v4dtrf6", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 7);

        // delete static var compensator
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.STATIC_VAR_COMPENSATOR.name());
        equipmentDeletionInfos.setEquipmentId("v3Compensator");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.STATIC_VAR_COMPENSATOR, "v3Compensator", "v3bCompensator", "v3dCompensator", "s2");
        testNetworkModificationsCount(TEST_GROUP_ID, 8);

        // delete battery
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.BATTERY.name());
        equipmentDeletionInfos.setEquipmentId("v3Battery");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.BATTERY, "v3Battery", "v3bBattery", "v3dBattery", "s2");
        testNetworkModificationsCount(TEST_GROUP_ID, 9);

        // delete dangling line
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.DANGLING_LINE.name());
        equipmentDeletionInfos.setEquipmentId("v2Dangling");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.DANGLING_LINE, "v2Dangling", "v2bdangling", "v2ddangling", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 10);

        // delete hvdc line => also delete converter stations
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.HVDC_LINE.name());
        equipmentDeletionInfos.setEquipmentId("hvdcLine");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testMultipleDeletionImpacts(IdentifiableType.HVDC_LINE,
                mvcResult.getResponse().getContentAsString(), "hvdcLine", List.of(),
                List.of(Pair.of(IdentifiableType.SWITCH, "v1blcc"), Pair.of(IdentifiableType.SWITCH, "v1dlcc"), Pair.of(IdentifiableType.HVDC_CONVERTER_STATION, "v1lcc"),
                        Pair.of(IdentifiableType.SWITCH, "v2bvsc"), Pair.of(IdentifiableType.SWITCH, "v2dvsc"), Pair.of(IdentifiableType.HVDC_CONVERTER_STATION, "v2vsc")
                ),
                "s1"
        );
        testNetworkModificationsCount(TEST_GROUP_ID, 11);

        // delete voltage level
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.VOLTAGE_LEVEL.name());
        equipmentDeletionInfos.setEquipmentId("v5");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testMultipleDeletionImpacts(IdentifiableType.VOLTAGE_LEVEL,
            mvcResult.getResponse().getContentAsString(), "v5", List.of("1A1"),
            List.of(Pair.of(IdentifiableType.GENERATOR, "v5generator"), Pair.of(IdentifiableType.LOAD, "v5load"),
                Pair.of(IdentifiableType.SHUNT_COMPENSATOR, "v5shunt"), Pair.of(IdentifiableType.STATIC_VAR_COMPENSATOR, "v5Compensator")),
            "s3"
        );
        testNetworkModificationsCount(TEST_GROUP_ID, 12);

        // try to delete voltage level (Internal error because the vl is still connected)
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.VOLTAGE_LEVEL.name());
        equipmentDeletionInfos.setEquipmentId("v4");
        mockMvc.perform(post(URI_NETWORK_MODIF).content(objectWriter.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertNotNull(network.getVoltageLevel("v4"));
        assertLogMessage(new PowsyblException(new AssertionError("The voltage level 'v4' cannot be removed because of a remaining LINE")).getMessage(),
                equipmentDeletionInfos.getErrorType().name(), reportService);

        // delete substation
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.SUBSTATION.name());
        equipmentDeletionInfos.setEquipmentId("s3");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        List<SimpleElementImpact> vlDeletionImpacts = testMultipleDeletionImpacts(IdentifiableType.VOLTAGE_LEVEL, "v6", List.of("1B1"),
            List.of(Pair.of(IdentifiableType.GENERATOR, "v6generator"), Pair.of(IdentifiableType.LOAD, "v6load"),
                Pair.of(IdentifiableType.SHUNT_COMPENSATOR, "v6shunt"), Pair.of(IdentifiableType.STATIC_VAR_COMPENSATOR, "v6Compensator")),
            "s3");
        testSubstationDeletionImpacts(mvcResult.getResponse().getContentAsString(), "s3", vlDeletionImpacts);
        testNetworkModificationsCount(TEST_GROUP_ID, 14);

        // try to delete substation (Internal error because the substation is still connected)
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.SUBSTATION.name());
        equipmentDeletionInfos.setEquipmentId("s2");
        mockMvc.perform(post(URI_NETWORK_MODIF).content(objectWriter.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(network.getSubstation("s2"));
        assertLogMessage("The substation s2 is still connected to another substation",
                equipmentDeletionInfos.getErrorType().name(), reportService);

        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID).isEmpty());
        assertEquals(55, tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID).size());
    }

    private void testConnectableDeletionImpacts(String resultAsString,
                                                IdentifiableType connectableType, String connectableId,
                                                String breakerId, String disconnectorId, String substationId) {
        TestImpactUtils.testConnectableDeletionImpacts(mapper, resultAsString, connectableType, connectableId, breakerId, disconnectorId, substationId);

        // Connectable and switches have been removed from network
        assertNull(network.getIdentifiable(connectableId));
        assertNull(network.getSwitch(breakerId));
        assertNull(network.getSwitch(disconnectorId));

        // Connectable and switches have been added as TombstonedEquipmentInfos in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(connectableId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(breakerId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(disconnectorId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    private void testBranchDeletionImpacts(String resultAsString,
                                           IdentifiableType branchType, String branchId,
                                           String breakerId1, String disconnectorId1, String substationId1,
                                           String breakerId2, String disconnectorId2, String substationId2) {
        TestImpactUtils.testBranchDeletionImpacts(mapper, resultAsString, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2);

        // line and switches have been removed from network
        assertNull(network.getLine(branchId));
        assertNull(network.getSwitch(breakerId1));
        assertNull(network.getSwitch(disconnectorId1));
        assertNull(network.getSwitch(breakerId2));
        assertNull(network.getSwitch(disconnectorId2));

        // line and switches have been added as TombstonedEquipmentInfos in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(branchId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(breakerId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(disconnectorId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(breakerId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(disconnectorId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    private void test3WTDeletionImpacts(String resultAsString, String w3tId,
                                        String breakerId1, String disconnectorId1,
                                        String breakerId2, String disconnectorId2,
                                        String breakerId3, String disconnectorId3,
                                        String substationId) {
        TestImpactUtils.test3WTDeletionImpacts(mapper, resultAsString, w3tId, breakerId1, disconnectorId1, breakerId2, disconnectorId2, breakerId3, disconnectorId3, substationId);

        // 3 windings transformer and switches have been removed from network
        assertNull(network.getThreeWindingsTransformer(w3tId));
        assertNull(network.getSwitch(breakerId1));
        assertNull(network.getSwitch(disconnectorId1));
        assertNull(network.getSwitch(breakerId2));
        assertNull(network.getSwitch(disconnectorId2));
        assertNull(network.getSwitch(breakerId3));
        assertNull(network.getSwitch(disconnectorId3));

        // 3 windings transformer and switches have been added as TombstonedEquipmentInfos in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(disconnectorId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(breakerId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(disconnectorId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(breakerId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(disconnectorId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(breakerId3, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(existTombstonedEquipmentInfos(disconnectorId3, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    private List<SimpleElementImpact> testMultipleDeletionImpacts(IdentifiableType equipmentType, String equipmentId, List<String> busbarSectionsIds, List<Pair<IdentifiableType, String>> connectablesTypesAndIds, String substationId) {
        // All equipments have been removed from network
        assertNull(network.getIdentifiable(equipmentId));
        busbarSectionsIds.forEach(id -> assertNull(network.getBusbarSection(id)));
        connectablesTypesAndIds.forEach(typeAndId -> assertNull(network.getBusbarSection(typeAndId.getRight())));

        // All equipments have been added as TombstonedEquipmentInfos in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(equipmentId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        busbarSectionsIds.forEach(id -> assertTrue(existTombstonedEquipmentInfos(id, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID)));
        connectablesTypesAndIds.forEach(typeAndId -> assertTrue(existTombstonedEquipmentInfos(typeAndId.getRight(), TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID)));

        return createMultipleDeletionImpacts(equipmentType, equipmentId, busbarSectionsIds, connectablesTypesAndIds, substationId);
    }

    private void testMultipleDeletionImpacts(IdentifiableType equipmentType, String resultAsString, String equipmentId, List<String> busbarSectionsIds, List<Pair<IdentifiableType, String>> connectablesTypesAndIds, String substationId) {
        List<SimpleElementImpact> testElementImpacts = testMultipleDeletionImpacts(equipmentType, equipmentId, busbarSectionsIds, connectablesTypesAndIds, substationId);
        TestImpactUtils.testElementImpacts(mapper, resultAsString, testElementImpacts);
    }

    private void testSubstationDeletionImpacts(String resultAsString, String subStationId, List<SimpleElementImpact> vlsDeletionImpacts) {
        List<SimpleElementImpact> impacts = new ArrayList<>(List.of(createDeletionImpactType(IdentifiableType.SUBSTATION, subStationId, Set.of(subStationId))));
        impacts.addAll(vlsDeletionImpacts);
        TestImpactUtils.testElementImpacts(mapper, resultAsString, impacts);

        // Substation and equipments have been removed from network and
        assertNull(network.getSubstation(subStationId));

        // Substation and equipments  have been  added as TombstonedEquipmentInfos in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(subStationId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
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

    @Test
    public void testGetLineTypesCatalog() throws Exception {
        // Exclude Id for those unit tests because it's exluded in dto
        EqualsVerifier.simple().forClass(LineTypeInfos.class).withIgnoredFields("id").verify();
        EqualsVerifier.simple().forClass(AerialLineTypeInfos.class).withIgnoredFields("id").verify();
        EqualsVerifier.simple().forClass(UndergroundLineTypeInfos.class).withIgnoredFields("id").verify();

        MvcResult mvcResult;
        String resultAsString;

        // Check if the catalog is empty
        mvcResult = mockMvc
                .perform(get(URI_LINE_CATALOG).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<LineTypeInfos> emptyLineTypes = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertEquals(0, emptyLineTypes.size());

        // Create the catalog with some line types
        String lineTypesCatalogJson1 = TestUtils.resourceToString(LINE_TYPES_CATALOG_JSON_FILE_1);
        mockMvc.perform(post(URI_LINE_CATALOG).content(lineTypesCatalogJson1).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // Check if the catalog is complete avoiding the duplicate entry
        mvcResult = mockMvc
                .perform(get(URI_LINE_CATALOG).contentType(MediaType.APPLICATION_JSON))
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<LineTypeInfos> lineTypes = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertEquals(8, lineTypes.size());

        // Check if catalog is completely updated
        String lineTypesCatalogJson2 = TestUtils.resourceToString(LINE_TYPES_CATALOG_JSON_FILE_2);
        mockMvc.perform(post(URI_LINE_CATALOG).content(lineTypesCatalogJson2).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        mvcResult = mockMvc
                .perform(get(URI_LINE_CATALOG).contentType(MediaType.APPLICATION_JSON))
                .andExpect(content().contentTypeCompatibleWith(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<LineTypeInfos> lineTypes2 = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertEquals(2, lineTypes2.size());

        mockMvc.perform(delete(URI_LINE_CATALOG))
                .andExpect(status().isOk());

        // Check if the catalog is empty
        mvcResult = mockMvc
                .perform(get(URI_LINE_CATALOG).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        emptyLineTypes = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertEquals(0, emptyLineTypes.size());
    }
}
