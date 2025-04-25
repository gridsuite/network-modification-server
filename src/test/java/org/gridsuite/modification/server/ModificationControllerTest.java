/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.JsonProcessingException;
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
import com.powsybl.network.store.client.PreloadingStrategy;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.LoadCreationInfos.LoadCreationInfosBuilder;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.TombstonedEquipmentInfosRepository;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.impacts.TestImpactUtils;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.utils.ModificationUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.NetworkModificationServerException.Type.DUPLICATION_ARGUMENT_INVALID;
import static org.gridsuite.modification.server.elasticsearch.EquipmentInfosService.TYPES_FOR_INDEXING;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@AutoConfigureMockMvc
@SpringBootTest
@Tag("IntegrationTest")
class ModificationControllerTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_ID_2 = UUID.fromString("7928181e-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_WITH_TEE_POINT_ID = UUID.fromString("1928181e-7974-4592-ba19-88027e4254e4");
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");
    private static final UUID TEST_NETWORK_WITH_FLUSH_ERROR_ID = UUID.fromString("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP2_ID = UUID.randomUUID();
    private static final UUID TEST_NETWORK_BUS_BREAKER_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_MIXED_TOPOLOGY_ID = UUID.fromString("22222222-7977-4592-ba19-88027e4254e4");
    private static final String VARIANT_NOT_EXISTING_ID = "variant_not_existing";
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_COMPOSITE_NETWORK_MODIF_BASE = "/v1/network-composite-modifications";
    private static final String URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT = "/v1/network-composite-modification/";
    private static final String URI_LINE_CATALOG = URI_NETWORK_MODIF_BASE + "/catalog/line_types";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_1 = "/lines-catalog.json";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_2 = "/line_types_catalog_2.json";
    private static final String NETWORK_MODIFICATION_URI = URI_NETWORK_MODIF_BASE + "?groupUuid=" + TEST_GROUP_ID;

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

    @BeforeEach
    void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);
        network2 = NetworkCreation.create(TEST_NETWORK_ID_2, false);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID_2), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network2);

        networkWithTeePoint = NetworkWithTeePoint.create(TEST_NETWORK_WITH_TEE_POINT_ID);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_WITH_TEE_POINT_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> networkWithTeePoint);

        when(networkStoreService.getNetwork(eq(NOT_FOUND_NETWORK_ID), nullable(PreloadingStrategy.class))).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_WITH_FLUSH_ERROR_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_WITH_FLUSH_ERROR_ID, true));

        networkBusBreaker = NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_BUS_BREAKER_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> networkBusBreaker);

        when(networkStoreService.getNetwork(eq(TEST_NETWORK_MIXED_TOPOLOGY_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> NetworkCreation.createMixedTopology(TEST_NETWORK_MIXED_TOPOLOGY_ID));

        doThrow(new PowsyblException()).when(networkStoreService).flush(argThat(n -> TEST_NETWORK_WITH_FLUSH_ERROR_ID.toString().equals(n.getId())));

        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @AfterEach
    void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    private boolean existTombstonedEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantId).stream().anyMatch(t -> t.getId().equals(equipmentId));
    }

    private void assertApplicationStatusOK(MvcResult mvcResult) throws Exception {
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().get(0).isPresent());
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, networkModificationsResult.modificationResults().get(0).get().getApplicationStatus());
    }

    private String getJsonBody(ModificationInfos modificationInfos, UUID networkUuid, String variantId) throws JsonProcessingException {
        return TestUtils.getJsonBody(modificationInfos, networkUuid, variantId);
    }

    private String getJsonBody(List<UUID> uuids, String variantId) throws JsonProcessingException {
        return TestUtils.getJsonBody(uuids, TEST_NETWORK_ID, variantId);
    }

    @Test
    void testModificationException() {
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR).getMessage(), MODIFICATION_ERROR.name());
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, "Error message").getMessage(), MODIFICATION_ERROR.name() + " : Error message");
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, new IllegalArgumentException("Error message")).getMessage(), MODIFICATION_ERROR.name() + " : Error message");
    }

    @Test
    void testEquipmentIdNonNull() {
        String errorMessage = "equipmentId is marked non-null but is null";
        LoadCreationInfosBuilder<?, ?> loadCreationBuilder = LoadCreationInfos.builder();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, loadCreationBuilder::build).getMessage());
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationBuilder.equipmentId(null)).getMessage());
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().equipmentId("idLoad").build();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationInfos.setEquipmentId(null)).getMessage());
    }

    @Test
    void testNetworkNotFound() throws Exception {
        String body = getJsonBody(LoadCreationInfos.builder().equipmentId("id").build(), NOT_FOUND_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mockMvc.perform(post(NETWORK_MODIFICATION_URI)
            .content(body)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage())
            );
    }

    @Test
    void assertThrowsUpdateModificationNotFound() {
        UUID modificationUuid = UUID.randomUUID();
        ModificationInfos modificationInfos = LoadCreationInfos.builder().equipmentId("id").build();
        String errorMessage = assertThrows(NetworkModificationException.class, () -> networkModificationService.updateNetworkModification(modificationUuid, modificationInfos)).getMessage();
        assertEquals(new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)).getMessage(), errorMessage);
        assertThrows(NullPointerException.class, () -> networkModificationService.updateNetworkModification(modificationUuid, null));
    }

    @Test
    void testModificationGroups() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);

        // no groups
        mvcResult = mockMvc.perform(get("/v1/groups")).andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON)).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<UUID> bsicListResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(bsicListResult, List.of());
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));

         // switch opening to create the default group
        mvcResult = mockMvc.perform(get("/v1/groups")).andExpectAll(
         status().isOk(),
         content().contentType(MediaType.APPLICATION_JSON))
         .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<UUID> bsicListResultUUID = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(bsicListResultUUID, List.of(TEST_GROUP_ID));
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", TEST_GROUP_ID)).andExpectAll(
         status().isOk(),
         content().contentType(MediaType.APPLICATION_JSON))
         .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsicListResulModifInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(1, bsicListResulModifInfos.size());
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true", TEST_GROUP_ID))
                        .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                        .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsicListResultInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(1, bsicListResultInfos.size());

        // delete the default modification group of a network
        mockMvc.perform(delete("/v1/groups/{groupUuid}", TEST_GROUP_ID))
                .andExpect(status().isOk());

        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true", TEST_GROUP_ID)).andExpectAll(status().isNotFound(),
                    content().string(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage()));

        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true&errorOnGroupNotFound=false", TEST_GROUP_ID)).andExpectAll(
         status().isOk(),
         content().contentType(MediaType.APPLICATION_JSON))
         .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsicListModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(bsicListModificationInfos, List.of());
    }

    @Test
    void testRestoreNetworkModifications() throws Exception {
        MvcResult mvcResult;
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .stashed(true)
                .build();
        String switchStatusModificationInfosJson = getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);

        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));

        List<ModificationInfos> modifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, modifications.size());

        String uuidString = modifications.get(0).getUuid().toString();
        mockMvc.perform(put(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", uuidString)
                        .queryParam("stashed", "false"))
                .andExpect(status().isOk());
        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, true, true, true).size());
    }

    @Test
    void testStashNetworkModifications() throws Exception {
        MvcResult mvcResult;
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .stashed(true)
                .build();
        String switchStatusModificationInfosJson = getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);

        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));

        List<ModificationInfos> modifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modifications.size());
        String uuidString = modifications.get(0).getUuid().toString();
        mockMvc.perform(put(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", uuidString)
                        .queryParam("stashed", "true"))
                .andExpect(status().isOk());
        assertEquals(true, modificationRepository.getModificationInfo(UUID.fromString(uuidString)).getStashed());
    }

    @Test
    void testDisableNetworkModifications() throws Exception {
        MvcResult mvcResult;
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .stashed(false)
            .activated(true)
            .build();
        String switchStatusModificationInfosJson = getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);

        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));

        List<ModificationInfos> modifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modifications.size());
        assertEquals(true, modifications.get(0).getActivated());

        String uuidString = modifications.get(0).getUuid().toString();
        mockMvc.perform(put(URI_NETWORK_MODIF_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", uuidString)
                .queryParam("activated", "false"))
            .andExpect(status().isOk());
        assertEquals(false, modificationRepository.getModifications(TEST_GROUP_ID, true, true).get(0).getActivated());
    }

    @Test
    void testDeleteModification() throws Exception {
        MvcResult mvcResult;
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));

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

    //test delete all modifications
    @Test
    void testDeleteAllModification() throws Exception {
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 3);

        assertEquals(3, modificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString()))
                .andExpect(status().isOk());
        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
    }

    @Test
    void testDeleteModificationMissingParamError() throws Exception {
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE))
                .andExpect(status().isInternalServerError())
                .andExpect(result -> assertInstanceOf(NetworkModificationException.class, result.getResolvedException()))
                .andExpect(result -> assertEquals("MODIFICATION_DELETION_ERROR : need to specify the group or give a list of UUIDs", result.getResolvedException().getMessage()));
    }

    @Test
    void testNetworkModificationsWithErrorOnNetworkFlush() throws Exception {

        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = getJsonBody(groovyScriptInfos, TEST_NETWORK_WITH_FLUSH_ERROR_ID, NetworkCreation.VARIANT_ID);

        mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().is5xxServerError());

        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, false).size());
    }

    @Test
    void testMultipleModificationsWithError() throws Exception {
        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptInfosJson = getJsonBody(groovyScriptInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        // apply groovy script without error
        MvcResult mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // apply groovy script with error on the second
        groovyScriptInfos.setScript("network.getGenerator('there is no generator').targetP=30\nnetwork.getGenerator('idGenerator').targetP=40\n");
        groovyScriptInfosJson = getJsonBody(groovyScriptInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON))
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
        MvcResult mvcResult;
        for (int i = 0; i < number; i++) {
            switchStatusModificationInfos.setEquipmentAttributeValue(openStates.get(i % 2));
            String bodyJson = getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
            mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BASE + "?groupUuid=" + groupId)
                            .content(bodyJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
            assertApplicationStatusOK(mvcResult);
        }
        var modificationList = modificationRepository.getModifications(groupId, false, true);
        assertEquals(number, modificationList.size());
        return modificationList;
    }

    private ModificationInfos createDeletionModification(UUID groupId, IdentifiableType equipmentType, String equipmentName) throws Exception {
        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType(equipmentType)
                .equipmentId(equipmentName)
                .build();
        String bodyJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        MvcResult mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BASE + "?groupUuid=" + groupId)
                            .content(bodyJson).contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        var modificationList = modificationRepository.getModifications(groupId, true, true);
        assertFalse(modificationList.isEmpty());
        return modificationList.get(modificationList.size() - 1);
    }

    @Test
    void testCopyModification() throws Exception {
        // create 3 modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 3);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Duplicate [0] and [1], and append them at the end of the group modification list.
        List<UUID> duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        List<UUID> badModificationUuidList = List.of(UUID.randomUUID(), UUID.randomUUID());
        duplicateModificationUuidList.addAll(badModificationUuidList);
        String bodyJson = getJsonBody(duplicateModificationUuidList, NetworkCreation.VARIANT_ID);
        MvcResult mvcResult = mockMvc.perform(
                put("/v1/groups/" + TEST_GROUP_ID + "?action=COPY")
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        var newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 5 modifications: first 0-1-2 are still the same, last 3-4 are new (duplicates of 0-1)
        assertEquals(5, newModificationList.size());
        assertEquals(modificationUuidList, newModificationUuidList.subList(0, 3));
        // compare duplicates 0 and 3 (same data except uuid)
        assertThat(newModificationList.get(3)).recursivelyEquals(modificationList.get(0));

        // compare duplicates 1 and 4 (same data except uuid)
        assertThat(newModificationList.get(4)).recursivelyEquals(modificationList.get(1));

        // bad request error case: wrong action param
        mockMvc.perform(
                put("/v1/groups/" + TEST_GROUP_ID + "?action=XXXXXXX")
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest());

        // create 1 modification in another group
        UUID otherGroupId = UUID.randomUUID();
        List<ModificationInfos> modificationListOtherGroup = createSomeSwitchModifications(otherGroupId, 1);
        List<UUID> modificationUuidListOtherGroup = modificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Duplicate the same modifications, and append them at the end of this new group modification list.
        duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        bodyJson = getJsonBody(duplicateModificationUuidList, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                put("/v1/groups/" + otherGroupId + "?action=COPY")
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        var newModificationListOtherGroup = modificationRepository.getModifications(otherGroupId, false, true);
        List<UUID> newModificationUuidListOtherGroup = newModificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 3 modifications in new group: first 0 is still the same, last 1-2 are new (duplicates of 0-1 from first group)
        assertEquals(3, newModificationListOtherGroup.size());
        assertEquals(modificationUuidListOtherGroup, newModificationUuidListOtherGroup.subList(0, 1));
        // compare duplicates
        assertThat(newModificationListOtherGroup.get(1)).recursivelyEquals(modificationList.get(0));
        assertThat(newModificationListOtherGroup.get(2)).recursivelyEquals(modificationList.get(1));

        // Duplicate all modifications in TEST_GROUP_ID, and append them at the end of otherGroupId
        bodyJson = getJsonBody(List.of(), NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                put("/v1/groups/" + otherGroupId + "?action=COPY" + "&originGroupUuid=" + TEST_GROUP_ID)
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        newModificationListOtherGroup = modificationRepository.getModifications(otherGroupId, true, true);
        // now 8 modifications in new group: first 3 are still the same, 5 last are new duplicates from first group
        assertEquals(8, newModificationListOtherGroup.size());

        // compare duplicates
        modificationList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        for (int i = 3; i < 8; ++i) {
            assertThat(newModificationListOtherGroup.get(i)).recursivelyEquals(modificationList.get(i - 3));
        }

        // Duplicate modifications from a group and from a list : illegal operation
        bodyJson = getJsonBody(duplicateModificationUuidList, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                put("/v1/groups/" + otherGroupId + "?action=COPY" + "&originGroupUuid=" + TEST_GROUP_ID)
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest()).andReturn();
        assertEquals(new NetworkModificationServerException(DUPLICATION_ARGUMENT_INVALID).getMessage(), mvcResult.getResponse().getContentAsString());
    }

    /**
     * TODO : Remove this test after the final integration of root networks
     * Need to use tne new test with modificationContextInfos DTO (see above)
     */
    @Test
    void testCopyModificationOld() throws Exception {
        // create 3 modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 3);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Duplicate [0] and [1], and append them at the end of the group modification list.
        List<UUID> duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        List<UUID> badModificationUuidList = List.of(UUID.randomUUID(), UUID.randomUUID());
        duplicateModificationUuidList.addAll(badModificationUuidList);
        String bodyJson = getJsonBody(duplicateModificationUuidList, NetworkCreation.VARIANT_ID);
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY";
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(
            put(url)
                .content(bodyJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        var newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 5 modifications: first 0-1-2 are still the same, last 3-4 are new (duplicates of 0-1)
        assertEquals(5, newModificationList.size());
        assertEquals(modificationUuidList, newModificationUuidList.subList(0, 3));
        // compare duplicates 0 and 3 (same data except uuid)
        assertThat(newModificationList.get(3)).recursivelyEquals(modificationList.get(0));

        // compare duplicates 1 and 4 (same data except uuid)
        assertThat(newModificationList.get(4)).recursivelyEquals(modificationList.get(1));

        // bad request error case: wrong action param
        String wrongUrl = "/v1/groups/" + TEST_GROUP_ID + "?action=XXXXXXX";
        mockMvc.perform(
                put(wrongUrl)
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest());

        // create 1 modification in another group
        UUID otherGroupId = UUID.randomUUID();
        List<ModificationInfos> modificationListOtherGroup = createSomeSwitchModifications(otherGroupId, 1);
        List<UUID> modificationUuidListOtherGroup = modificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Duplicate the same modifications, and append them at the end of this new group modification list.
        duplicateModificationUuidList = new ArrayList<>(modificationUuidList.subList(0, 2));
        String copyUrl = "/v1/groups/" + otherGroupId + "?action=COPY";
        bodyJson = getJsonBody(duplicateModificationUuidList, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                put(copyUrl)
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        var newModificationListOtherGroup = modificationRepository.getModifications(otherGroupId, false, true);
        List<UUID> newModificationUuidListOtherGroup = newModificationListOtherGroup.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // now 3 modifications in new group: first 0 is still the same, last 1-2 are new (duplicates of 0-1 from first group)
        assertEquals(3, newModificationListOtherGroup.size());
        assertEquals(modificationUuidListOtherGroup, newModificationUuidListOtherGroup.subList(0, 1));
        // compare duplicates
        assertThat(newModificationListOtherGroup.get(1)).recursivelyEquals(modificationList.get(0));
        assertThat(newModificationListOtherGroup.get(2)).recursivelyEquals(modificationList.get(1));

        // Duplicate all modifications in TEST_GROUP_ID, and append them at the end of otherGroupId
        String bodyJson2 = getJsonBody(List.of(), NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                put("/v1/groups/" + otherGroupId + "?action=COPY" + "&originGroupUuid=" + TEST_GROUP_ID)
                    .content(bodyJson2)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        newModificationListOtherGroup = modificationRepository.getModifications(otherGroupId, true, true);
        // now 8 modifications in new group: first 3 are still the same, 5 last are new duplicates from first group
        assertEquals(8, newModificationListOtherGroup.size());

        // compare duplicates
        modificationList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        for (int i = 3; i < 8; ++i) {
            assertThat(newModificationListOtherGroup.get(i)).recursivelyEquals(modificationList.get(i - 3));
        }
    }

    @Test
    void testCopyModificationWithUnexistingId() throws Exception {
        // create 1 modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // Try to copy an unexisting Modification
        List<UUID> duplicateModificationUuidList = List.of(UUID.randomUUID());
        String bodyJson = getJsonBody(duplicateModificationUuidList, NetworkCreation.VARIANT_ID);
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY" + "&before=" + modificationUuidList.get(0);
        mockMvc.perform(put(url).content(bodyJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        // we still have the same and only modification
        assertEquals(newModificationUuidList, modificationUuidList);
    }

    @Test
    void createGeneratorWithStartup() throws Exception {
        // create and build generator without startup
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v2", "idGenerator1", "nameGenerator1", "1B", "v2load", "LOAD", "v1");
        String generatorCreationInfosJson = getJsonBody(generatorCreationInfos, TEST_NETWORK_ID, null);
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        GeneratorStartup generatorStartup = network.getGenerator("idGenerator1").getExtension(GeneratorStartup.class);
        assertNull(generatorStartup);

        // same for bus breaker
        GeneratorCreationInfos generatorCreationInfosBusBreaker = ModificationCreation.getCreationGenerator("v1", "idGenerator2", "nameGenerator2", "bus1", "idGenerator1", "GENERATOR", "v1");
        generatorCreationInfosJson = getJsonBody(generatorCreationInfosBusBreaker, TEST_NETWORK_BUS_BREAKER_ID, null);

        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        generatorStartup = networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID, null).getGenerator("idGenerator2").getExtension(GeneratorStartup.class);
        assertNull(generatorStartup);

        // create and build generator with startup
        generatorCreationInfos.setEquipmentId("idGenerator21");
        generatorCreationInfos.setMarginalCost(8.);
        generatorCreationInfosJson = getJsonBody(generatorCreationInfos, TEST_NETWORK_ID, null);

        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        generatorStartup = network.getGenerator("idGenerator21").getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup);
        assertEquals(Double.NaN, generatorStartup.getPlannedActivePowerSetpoint(), 0);
        assertEquals(8., generatorStartup.getMarginalCost(), 0);
        assertEquals(Double.NaN, generatorStartup.getPlannedOutageRate(), 0);
        assertEquals(Double.NaN, generatorStartup.getForcedOutageRate(), 0);

        // same for bus breaker
        generatorCreationInfosBusBreaker.setEquipmentId("idGenerator3");
        generatorCreationInfosBusBreaker.setPlannedOutageRate(80.);
        generatorCreationInfosJson = getJsonBody(generatorCreationInfosBusBreaker, TEST_NETWORK_BUS_BREAKER_ID, null);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        generatorStartup = networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID, null).getGenerator("idGenerator3").getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup);
        assertEquals(Double.NaN, generatorStartup.getPlannedActivePowerSetpoint(), 0);
        assertEquals(Double.NaN, generatorStartup.getMarginalCost(), 0);
        assertEquals(80., generatorStartup.getPlannedOutageRate(), 0);
        assertEquals(Double.NaN, generatorStartup.getForcedOutageRate(), 0);
    }

    @Test
    void testMoveModificationInSameGroup() throws Exception {
        // create 2 modifications in a single group
        List<UUID> modificationUuidList = createSomeSwitchModifications(TEST_GROUP_ID, 2).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // swap modifications: move [1] before [0]
        List<UUID> movingModificationUuidList = List.of(modificationUuidList.get(1));
        String bodyJson = getJsonBody(movingModificationUuidList, NetworkCreation.VARIANT_ID);
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=MOVE" + "&before=" + modificationUuidList.get(0);
        mockMvc.perform(put(url).content(bodyJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var newModificationUuidList = modificationRepository.getModifications(TEST_GROUP_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newModificationUuidList);
        Collections.reverse(newModificationUuidList); // swap => reverse order is expected
        assertEquals(modificationUuidList, newModificationUuidList);
    }

    @Test
    void testNetworkCompositeModification() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 2;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);
        assertEquals(modificationsNumber, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // Create a composite modification with the switch modification
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        ModificationInfos compositeModificationInfos = CompositeModificationInfos.builder()
            .modifications(modificationList)
            .build();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(modificationRepository.getModificationInfo(compositeModificationUuid)).recursivelyEquals(compositeModificationInfos);

        // get the modification infos (metadata only)
        List<ModificationInfos> modificationInfosList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(modificationsNumber, modificationInfosList.size());
        // get the composite modification (metadata only)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + compositeModificationUuid + "/network-modifications"))
            .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> compositeModificationContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(modificationsNumber, compositeModificationContent.size());
        for (int i = 0; i < modificationUuids.size(); i++) {
            assertEquals(modificationInfosList.get(i).getMessageValues(), compositeModificationContent.get(i).getMessageValues());
        }
        assertNotNull(compositeModificationContent.get(0).getMessageType());
        assertNotNull(compositeModificationContent.get(0).getMessageValues());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeName());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeValue());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentType());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentId());

        // get the composite modification (complete data)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + compositeModificationUuid + "/network-modifications?onlyMetadata=false"))
            .andExpect(status().isOk()).andReturn();
        compositeModificationContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals("open", ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeName());
        assertEquals(Boolean.TRUE, ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeValue());
        assertEquals(IdentifiableType.SWITCH, ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentType());
        assertEquals("v1b1", ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentId());

        // Insert the composite modification in the group
        String bodyJson = getJsonBody(List.of(compositeModificationUuid), NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                put("/v1/groups/" + TEST_GROUP_ID + "?action=INSERT")
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        List<ModificationInfos> newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(modificationsNumber * 2, newModificationList.size());
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).toList();
        assertEquals(modificationUuids.get(0), newModificationUuidList.get(0));
        assertThat(modificationList.get(0)).recursivelyEquals(newModificationList.get(modificationsNumber));
    }

    /**
     * TODO : Remove this test after the final integration of root networks
     * Need to use tne new test with modificationContextInfos DTO (see above)
     */
    @Test
    void testNetworkCompositeModificationOld() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 2;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);
        assertEquals(modificationsNumber, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // Create a composite modification with the switch modification
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        ModificationInfos compositeModificationInfos = CompositeModificationInfos.builder()
                .modifications(modificationList)
                .build();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(modificationRepository.getModificationInfo(compositeModificationUuid)).recursivelyEquals(compositeModificationInfos);
        assertEquals(modificationsNumber, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // get the composite modification (metadata only)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + compositeModificationUuid + "/network-modifications"))
                .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> compositeModificationContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(modificationsNumber, compositeModificationContent.size());
        assertNotNull(compositeModificationContent.get(0).getMessageType());
        assertNotNull(compositeModificationContent.get(0).getMessageValues());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeName());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeValue());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentType());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentId());

        // get the composite modification (complete data)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + compositeModificationUuid + "/network-modifications?onlyMetadata=false"))
            .andExpect(status().isOk()).andReturn();
        compositeModificationContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals("open", ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeName());
        assertEquals(Boolean.TRUE, ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentAttributeValue());
        assertEquals(IdentifiableType.SWITCH, ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentType());
        assertEquals("v1b1", ((EquipmentAttributeModificationInfos) compositeModificationContent.get(0)).getEquipmentId());
        String bodyJson = getJsonBody(List.of(compositeModificationUuid), NetworkCreation.VARIANT_ID);
        // Insert the composite modification in the group
        mvcResult = mockMvc.perform(
                        put("/v1/groups/" + TEST_GROUP_ID + "?action=INSERT")
                                .content(bodyJson)
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        List<ModificationInfos> newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(modificationsNumber * 2, newModificationList.size());
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).toList();
        assertEquals(modificationUuids.get(0), newModificationUuidList.get(0));
        assertThat(modificationList.get(0)).recursivelyEquals(newModificationList.get(modificationsNumber));
    }

    @Test
    void testMoveModificationBetweenTwoGroups() throws Exception {
        String substationS3 = network.getLoad("v5load").getTerminal().getVoltageLevel().getSubstation().get().getId();
        String substationS1 = network.getLoad("v1load").getTerminal().getVoltageLevel().getSubstation().get().getId();
        assertEquals("s3", substationS3);
        assertEquals("s1", substationS1);

        // create 4 modifications in destination group (3 switch updates + 1 load deletion in s3)
        List<UUID> destinationModificationUuidList = ListUtils.union(
                createSomeSwitchModifications(TEST_GROUP_ID, 3).stream().map(ModificationInfos::getUuid).toList(),
                List.of(createDeletionModification(TEST_GROUP_ID, IdentifiableType.LOAD, "v5load").getUuid())
                );
        // create 1 modification in origin group (1 load deletion in s1)
        UUID originSingleModification = createDeletionModification(TEST_GROUP2_ID, IdentifiableType.LOAD, "v1load").getUuid();

        // cut origin[0] and append to destination
        List<UUID> movingModificationUuidList = List.of(originSingleModification);
        String bodyJson = getJsonBody(movingModificationUuidList, NetworkCreation.VARIANT_ID);
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=MOVE" + "&originGroupUuid=" + TEST_GROUP2_ID + "&build=true";
        MvcResult mvcResult = mockMvc.perform(put(url).content(bodyJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        // incremental build: deletion impacts expected, all related to the moved load deletion (dealing with "s1" substation)
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().get(0).isPresent());
        networkModificationsResult.modificationResults().get(0).get().getNetworkImpacts().forEach(i -> {
            assertTrue(i.isSimple());
            SimpleElementImpact simpleImpact = (SimpleElementImpact) i;
            assertEquals(Set.of(substationS1), simpleImpact.getSubstationIds());
            assertEquals(SimpleElementImpact.SimpleImpactType.DELETION, simpleImpact.getSimpleImpactType());
        });

        // check destination
        var newDestinationModificationUuidList = modificationRepository.getModifications(TEST_GROUP_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newDestinationModificationUuidList);
        // Expect: existing list + the moved one
        List<UUID> expectedDestinationModificationUuidList = ListUtils.union(destinationModificationUuidList, movingModificationUuidList);
        assertEquals(expectedDestinationModificationUuidList, newDestinationModificationUuidList);

        // check origin
        var newOriginModificationUuidList = modificationRepository.getModifications(TEST_GROUP2_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newOriginModificationUuidList);
        // Expect: empty
        assertEquals(List.of(), newOriginModificationUuidList);
    }

    @Test
    void testMoveModificationWithUnexistingId() throws Exception {
        // create 2 modifications
        List<UUID> modificationUuidList = createSomeSwitchModifications(TEST_GROUP_ID, 2).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // try to move an unexisting modification before [0]: no error, no change
        List<UUID> movingModificationUuidList = List.of(UUID.randomUUID());
        String bodyJson = getJsonBody(movingModificationUuidList, NetworkCreation.VARIANT_ID);
        String url = "/v1/groups/" + TEST_GROUP_ID + "?action=MOVE" + "&originGroupUuid=" + TEST_GROUP_ID + "&before=" + modificationUuidList.get(0);

        mockMvc.perform(put(url).content(bodyJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var newModificationUuidList = modificationRepository.getModifications(TEST_GROUP_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newModificationUuidList);
        // nothing has changed in modification group
        assertEquals(modificationUuidList, newModificationUuidList);
    }

    @Test
    void testDuplicateModificationGroup() throws Exception {
        MvcResult mvcResult;
        VoltageLevelCreationInfos vl1 = ModificationCreation.getCreationVoltageLevel("s1", "vl1Id", "vl1Name");
        String bodyJson = getJsonBody(vl1, TEST_NETWORK_BUS_BREAKER_ID, null);
        mockMvc.perform(
                post(NETWORK_MODIFICATION_URI)
                    .content(bodyJson)
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
                .r(100.0)
                .x(100.0)
                .g1(10.0)
                .b1(10.0)
                .g2(20.0)
                .b2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(c1).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(c2).build()
                    )
                )
                .build();
        bodyJson = getJsonBody(lineCreationInfos, TEST_NETWORK_BUS_BREAKER_ID, null);

        mvcResult = mockMvc.perform(
            post(NETWORK_MODIFICATION_URI)
                .content(bodyJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();
        assertApplicationStatusOK(mvcResult);
        String resultAsString = mvcResult.getResponse().getContentAsString();
        testBranchCreationImpacts(mapper, resultAsString, Set.of("s1", "s2"));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        //create a lineAttached
        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .equipmentId("attachmentLine")
                .r(50.6)
                .x(25.3)
                .build();

        LineAttachToVoltageLevelInfos lineAttachToVL = new LineAttachToVoltageLevelInfos("line3",
                10.0, "AttPointId", "attPointName", null, "v4",
                "1.A", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");
        String bodyJson2 = getJsonBody(lineAttachToVL, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                post(NETWORK_MODIFICATION_URI)
                    .content(bodyJson2)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        //create a lineSplit
        LineSplitWithVoltageLevelInfos lineSplitWoVL = new LineSplitWithVoltageLevelInfos("line1", 10.0, null, "v4", "1.A",
                "nl11", "NewLine11", "nl12", "NewLine12");
        bodyJson2 = getJsonBody(lineSplitWoVL, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                post(NETWORK_MODIFICATION_URI)
                    .content(bodyJson2)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        //create a generator
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v2", "idGenerator1", "nameGenerator1", "1B", "v2load", "LOAD", "v1");
        bodyJson2 = getJsonBody(generatorCreationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(
                post(NETWORK_MODIFICATION_URI)
                    .content(bodyJson2)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        // get list of modifications
        List<ModificationInfos> modifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(5, modifications.size());
        //stash the first modification
        String uuidString = modifications.get(0).getUuid().toString();

        mockMvc.perform(put(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", uuidString)
                        .queryParam("stashed", "true"))
                .andExpect(status().isOk());
        List<ModificationInfos> stashedModifications = modificationRepository.getModificationsMetadata(TEST_GROUP_ID, true);
        List<ModificationInfos> modificationAfterStash = modificationRepository.getModificationsMetadata(TEST_GROUP_ID, false)
                .stream().filter(modificationInfos -> !modificationInfos.getStashed()).toList();
        assertEquals(1, stashedModifications.size());
        assertEquals(4, modificationAfterStash.size());

        //test copy group with stashed modification
        UUID newGroupUuid = UUID.randomUUID();
        String uriStringGroups = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(uriStringGroups)).andExpect(status().isOk());
        List<ModificationInfos> stashedCopiedModifications = modificationRepository.getModificationsMetadata(newGroupUuid, true);
        List<ModificationInfos> copiedModifications = modificationRepository.getModificationsMetadata(newGroupUuid, false);
        assertEquals(0, stashedCopiedModifications.size());
        assertEquals(4, copiedModifications.size());
        testNetworkModificationsCount(newGroupUuid, 4);
    }

    @Test
    void replaceTeePointByVoltageLevelOnLineDuplicateModificationGroupTest() throws Exception {
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = new LinesAttachToSplitLinesInfos("l1", "l2", "l3", "v4", "bbs4", "nl1", "NewLine1", "nl2", "NewLine2");

        String body = getJsonBody(linesAttachToSplitLinesInfos, TEST_NETWORK_WITH_TEE_POINT_ID, null);

        MvcResult mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI)
                                .content(body)
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // test copy group
        UUID newGroupUuid = UUID.randomUUID();
        String copyGroupUriString = "/v1/groups?groupUuid=" + newGroupUuid + "&duplicateFrom=" + TEST_GROUP_ID + "&reportUuid=" + UUID.randomUUID();
        mockMvc.perform(post(copyGroupUriString))
                .andExpect(status().isOk());

        testNetworkModificationsCount(newGroupUuid, 1);
    }

    @Test
    void testGroupDuplication() throws Exception {
        // create new load in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
                .equipmentId("idLoad1")
                .equipmentName("nameLoad1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .loadType(LoadType.AUXILIARY)
                .p0(100.0)
                .q0(60.0)
                .connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionName("bottom")
                .build();
        String loadCreationInfosJson = getJsonBody(loadCreationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        MvcResult mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
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
    void testTombstonedEquipmentInfos() throws Exception {
        MvcResult mvcResult;

        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).isEmpty());
        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID).isEmpty());
        assertTrue(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).isEmpty());
        assertTrue(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID).isEmpty());

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType(IdentifiableType.LOAD)
                .equipmentId("v1load")
                .build();
        String equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);

        // delete load
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.LOAD, "v1load", "v1b1", "v1d1", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test delete load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be deleted
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.LOAD);
        equipmentDeletionInfos.setEquipmentId("v3load");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VARIANT_NOT_EXISTING_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationsResult> networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationsResult.isPresent());
        assertFalse(networkModificationsResult.get().modificationResults().isEmpty());
        assertEquals(1, networkModificationsResult.get().modificationResults().size());
        assertTrue(networkModificationsResult.get().modificationResults().getFirst().isEmpty());  // no modification apply
        assertNotNull(network.getLoad("v3load"));  // load was not deleted
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // delete shunt compensator
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.SHUNT_COMPENSATOR);
        equipmentDeletionInfos.setEquipmentId("v2shunt");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.SHUNT_COMPENSATOR, "v2shunt", "v2bshunt", "v2dshunt", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        // delete generator
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.GENERATOR);
        equipmentDeletionInfos.setEquipmentId("idGenerator");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.GENERATOR, "idGenerator", "v2bgenerator", "v2dgenerator", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        // delete line
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.LINE);
        equipmentDeletionInfos.setEquipmentId("line2");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
             .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testBranchDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.LINE, "line2", "v1bl2", "v1dl2", "s1", "v3bl2", "v3dl2", "s2");
        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        // delete two windings transformer
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.TWO_WINDINGS_TRANSFORMER);
        equipmentDeletionInfos.setEquipmentId("trf1");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testBranchDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.TWO_WINDINGS_TRANSFORMER, "trf1", "v1btrf1", "v1dtrf1", "s1", "v2btrf1", "v2dtrf1", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 6);

        // delete three windings transformer
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.THREE_WINDINGS_TRANSFORMER);
        equipmentDeletionInfos.setEquipmentId("trf6");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        test3WTDeletionImpacts(mvcResult.getResponse().getContentAsString(), "trf6", "v1btrf6", "v1dtrf6", "v2btrf6", "v2dtrf6", "v4btrf6", "v4dtrf6", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 7);

        // delete static var compensator
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.STATIC_VAR_COMPENSATOR);
        equipmentDeletionInfos.setEquipmentId("v3Compensator");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.STATIC_VAR_COMPENSATOR, "v3Compensator", "v3bCompensator", "v3dCompensator", "s2");
        testNetworkModificationsCount(TEST_GROUP_ID, 8);

        // delete battery
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.BATTERY);
        equipmentDeletionInfos.setEquipmentId("v3Battery");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.BATTERY, "v3Battery", "v3bBattery", "v3dBattery", "s2");
        testNetworkModificationsCount(TEST_GROUP_ID, 9);

        // delete dangling line
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.DANGLING_LINE);
        equipmentDeletionInfos.setEquipmentId("v2Dangling");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        testConnectableDeletionImpacts(mvcResult.getResponse().getContentAsString(), IdentifiableType.DANGLING_LINE, "v2Dangling", "v2bdangling", "v2ddangling", "s1");
        testNetworkModificationsCount(TEST_GROUP_ID, 10);

        // delete hvdc line => also delete converter stations
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.HVDC_LINE);
        equipmentDeletionInfos.setEquipmentId("hvdcLine");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        List<AbstractBaseImpact> expectedImpacts = createMultipleDeletionImpacts(
            List.of(
                Pair.of(IdentifiableType.HVDC_LINE, "hvdcLine"),
                Pair.of(IdentifiableType.HVDC_CONVERTER_STATION, "v1lcc"), Pair.of(IdentifiableType.HVDC_CONVERTER_STATION, "v2vsc"),
                Pair.of(IdentifiableType.SWITCH, "v1blcc"), Pair.of(IdentifiableType.SWITCH, "v1dlcc"),
                Pair.of(IdentifiableType.SWITCH, "v2bvsc"), Pair.of(IdentifiableType.SWITCH, "v2dvsc")
            ),
            Set.of("s1")
        );
        testMultipleDeletionImpacts(mvcResult.getResponse().getContentAsString(), expectedImpacts);
        testNetworkModificationsCount(TEST_GROUP_ID, 11);

        // delete voltage level
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.VOLTAGE_LEVEL);
        equipmentDeletionInfos.setEquipmentId("v5");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        expectedImpacts = createMultipleDeletionImpacts(
            List.of(
                Pair.of(IdentifiableType.VOLTAGE_LEVEL, "v5"), Pair.of(IdentifiableType.BUSBAR_SECTION, "1A1"),
                Pair.of(IdentifiableType.GENERATOR, "v5generator"), Pair.of(IdentifiableType.LOAD, "v5load"),
                Pair.of(IdentifiableType.SHUNT_COMPENSATOR, "v5shunt"), Pair.of(IdentifiableType.STATIC_VAR_COMPENSATOR, "v5Compensator")
            ),
            Set.of("s3")
        );
        testMultipleDeletionImpacts(mvcResult.getResponse().getContentAsString(), expectedImpacts);
        testNetworkModificationsCount(TEST_GROUP_ID, 12);

        // delete substation
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.SUBSTATION);
        equipmentDeletionInfos.setEquipmentId("s3");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        expectedImpacts = createMultipleDeletionImpacts(
            List.of(
                Pair.of(IdentifiableType.SUBSTATION, "s3"),
                Pair.of(IdentifiableType.VOLTAGE_LEVEL, "v6"), Pair.of(IdentifiableType.BUSBAR_SECTION, "1B1"),
                Pair.of(IdentifiableType.GENERATOR, "v6generator"), Pair.of(IdentifiableType.LOAD, "v6load"),
                Pair.of(IdentifiableType.SHUNT_COMPENSATOR, "v6shunt"), Pair.of(IdentifiableType.STATIC_VAR_COMPENSATOR, "v6Compensator")
            ),
            Set.of("s3")
        );
        testMultipleDeletionImpacts(mvcResult.getResponse().getContentAsString(), expectedImpacts);
        testNetworkModificationsCount(TEST_GROUP_ID, 13);

        // delete substation with lines
        equipmentDeletionInfos.setEquipmentType(IdentifiableType.SUBSTATION);
        equipmentDeletionInfos.setEquipmentId("s2");
        equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);
        expectedImpacts = new ArrayList<>();
        // Two winding transformer trf2 (in s1) is regulating on v3load (in s2), resetting regulation on delete of v3load adds a modification impact on the substation s1
        expectedImpacts.add(createModificationImpactType(IdentifiableType.SUBSTATION, "s1", Set.of("s1")));
        expectedImpacts.addAll(createMultipleDeletionImpacts(
            List.of(
                Pair.of(IdentifiableType.SUBSTATION, "s2"), Pair.of(IdentifiableType.VOLTAGE_LEVEL, "v3"),
                Pair.of(IdentifiableType.BUSBAR_SECTION, "3A"), Pair.of(IdentifiableType.LOAD, "v3load"),
                Pair.of(IdentifiableType.SWITCH, "v3d1"), Pair.of(IdentifiableType.SWITCH, "v3b1"),
                Pair.of(IdentifiableType.SWITCH, "v3bl1"), Pair.of(IdentifiableType.SWITCH, "v3dl1"),
                Pair.of(IdentifiableType.SWITCH, "v3bl3"), Pair.of(IdentifiableType.SWITCH, "v3dl3")
            ),
            Set.of("s2")
        ));
        expectedImpacts.addAll(createMultipleDeletionImpacts(
            List.of(
                Pair.of(IdentifiableType.SWITCH, "v1bl3"), Pair.of(IdentifiableType.SWITCH, "v1dl3"),
                Pair.of(IdentifiableType.SWITCH, "v4bl1"), Pair.of(IdentifiableType.SWITCH, "v4dl1")
            ),
            Set.of("s1")
        ));
        expectedImpacts.addAll(createMultipleDeletionImpacts(
            List.of(Pair.of(IdentifiableType.LINE, "line1"), Pair.of(IdentifiableType.LINE, "line3")), Set.of("s1", "s2")
        ));
        testMultipleDeletionImpacts(mvcResult.getResponse().getContentAsString(), expectedImpacts);
        testNetworkModificationsCount(TEST_GROUP_ID, 14);

        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID).isEmpty());
        // switches and bus bar sections are not indexed in ElasticSearch
        assertEquals(28, tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID).size());
    }

    private void testConnectableDeletionImpacts(String resultAsString,
                                                IdentifiableType connectableType, String connectableId,
                                                String breakerId, String disconnectorId, String substationId) throws JsonProcessingException {
        TestImpactUtils.testConnectableDeletionImpacts(mapper, resultAsString, connectableType, connectableId, breakerId, disconnectorId, substationId);

        // Connectable and switches have been removed from network
        assertNull(network.getIdentifiable(connectableId));
        assertNull(network.getSwitch(breakerId));
        assertNull(network.getSwitch(disconnectorId));

        // Connectable have been added as TombstonedEquipmentInfos in ElasticSearch
        // switches are not indexed in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(connectableId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(breakerId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    private void testBranchDeletionImpacts(String resultAsString,
                                           IdentifiableType branchType, String branchId,
                                           String breakerId1, String disconnectorId1, String substationId1,
                                           String breakerId2, String disconnectorId2, String substationId2) throws JsonProcessingException {
        TestImpactUtils.testBranchDeletionImpacts(mapper, resultAsString, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2);

        // line and switches have been removed from network
        assertNull(network.getBranch(branchId));
        assertNull(network.getSwitch(breakerId1));
        assertNull(network.getSwitch(disconnectorId1));
        assertNull(network.getSwitch(breakerId2));
        assertNull(network.getSwitch(disconnectorId2));

        // line have been added as TombstonedEquipmentInfos in ElasticSearch
        // switches are not indexed in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(branchId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(breakerId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(breakerId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    private void test3WTDeletionImpacts(String resultAsString, String w3tId,
                                        String breakerId1, String disconnectorId1,
                                        String breakerId2, String disconnectorId2,
                                        String breakerId3, String disconnectorId3,
                                        String substationId) throws JsonProcessingException {
        TestImpactUtils.test3WTDeletionImpacts(mapper, resultAsString, w3tId, breakerId1, disconnectorId1, breakerId2, disconnectorId2, breakerId3, disconnectorId3, substationId);

        // 3 windings transformer and switches have been removed from network
        assertNull(network.getThreeWindingsTransformer(w3tId));
        assertNull(network.getSwitch(breakerId1));
        assertNull(network.getSwitch(disconnectorId1));
        assertNull(network.getSwitch(breakerId2));
        assertNull(network.getSwitch(disconnectorId2));
        assertNull(network.getSwitch(breakerId3));
        assertNull(network.getSwitch(disconnectorId3));

        // 3 windings transformer have been added as TombstonedEquipmentInfos in ElasticSearch
        // switches are not indexed in ElasticSearch
        assertTrue(existTombstonedEquipmentInfos(w3tId, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(breakerId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId1, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(breakerId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId2, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(breakerId3, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(existTombstonedEquipmentInfos(disconnectorId3, TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    private void testMultipleDeletionImpacts(String networkModificationResultAsString, List<AbstractBaseImpact> expectedImpacts) throws Exception {
        for (AbstractBaseImpact impact : expectedImpacts) {
            if (impact instanceof SimpleElementImpact simpleImpact && simpleImpact.isDeletion()) {
                // Equipment has been removed from network
                assertNull(network.getIdentifiable(simpleImpact.getElementId()));

                // Equipment has been added as TombstonedEquipmentInfos in ElasticSearch except for excluded types
                if (TYPES_FOR_INDEXING.contains(simpleImpact.getElementType())) {
                    assertTrue(existTombstonedEquipmentInfos(simpleImpact.getElementId(), TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
                } else {
                    assertFalse(existTombstonedEquipmentInfos(simpleImpact.getElementId(), TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
                }
            }
        }

        TestImpactUtils.testElementImpacts(mapper, networkModificationResultAsString, expectedImpacts);
    }

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        // get all modifications for the given group of a network
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true", groupUuid).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> modificationsTestGroupId = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(actualSize, modificationsTestGroupId.size());
    }

    @Test
    void shouldGetPosition() {
        var network = networkStoreService.getNetwork(TEST_NETWORK_ID, null);
        var network2 = networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID, null);
        var vl = network.getVoltageLevel("v2");
        var vl2 = network2.getVoltageLevel("v2");
        assertEquals(11, vl.getConnectableCount());
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
    void testGetPositionAfterAndBefore() {
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
    void testGetLineTypesCatalog() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // Check if the catalog is empty
        mvcResult = mockMvc
                .perform(get(URI_LINE_CATALOG).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<LineTypeInfos> emptyLineTypes = mapper.readValue(resultAsString, new TypeReference<>() { });
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
        List<LineTypeInfos> lineTypes = mapper.readValue(resultAsString, new TypeReference<>() { });
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
        List<LineTypeInfos> lineTypes2 = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(2, lineTypes2.size());

        mockMvc.perform(delete(URI_LINE_CATALOG))
                .andExpect(status().isOk());

        // Check if the catalog is empty
        mvcResult = mockMvc
                .perform(get(URI_LINE_CATALOG).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        emptyLineTypes = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(0, emptyLineTypes.size());
    }

    @Test
    void testCreateVoltageInitModification() throws Exception {
        // Create the modification
        VoltageInitModificationInfos modificationsInfos1 = VoltageInitModificationInfos.builder()
            .stashed(false)
            .generators(List.of(
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("G1")
                    .targetQ(10.)
                    .build(),
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("G2")
                    .targetV(226.)
                    .build()))
            .transformers(List.of(
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("2WT1")
                    .ratioTapChangerPosition(3)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("3WT1")
                    .ratioTapChangerPosition(1)
                    .ratioTapChangerTargetV(225.)
                    .legSide(ThreeSides.TWO)
                    .build()))
            .staticVarCompensators(List.of(
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("SVC1")
                    .reactivePowerSetpoint(50.)
                    .build(),
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("SVC2")
                    .voltageSetpoint(374.)
                    .build()))
            .vscConverterStations(List.of(
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("VSC1")
                    .reactivePowerSetpoint(40.)
                    .build(),
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("VSC2")
                    .voltageSetpoint(224.)
                    .build()))
            .shuntCompensators(List.of(
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v2shunt")
                    .sectionCount(1)
                    .connect(true)
                    .targetV(225.)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v5shunt")
                    .sectionCount(0)
                    .connect(false)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v6shunt")
                    .sectionCount(1)
                    .connect(false)
                    .targetV(380.)
                    .build()))
            .buses(List.of(
                VoltageInitBusModificationInfos.builder()
                    .voltageLevelId("v1")
                    .busId("1.1")
                    .v(225.)
                    .angle(0.)
                    .build(),
                VoltageInitBusModificationInfos.builder()
                    .voltageLevelId("v1")
                    .busId("1.2")
                    .v(226.)
                    .angle(0.6)
                    .build()))
            .build();

        UUID groupUuid = UUID.randomUUID();
        mockMvc.perform(post(URI_NETWORK_MODIF_BASE)
                .queryParam("groupUuid", groupUuid.toString())
                .content(objectWriter.writeValueAsString(org.springframework.data.util.Pair.of(modificationsInfos1, List.of())))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        // Get the modifications
        MvcResult mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=false", groupUuid)).andExpectAll(
                status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        List<VoltageInitModificationInfos> modificationsInfos2 = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, modificationsInfos2.size());
        assertThat(modificationsInfos2.get(0)).recursivelyEquals(modificationsInfos1);
    }

    @Test
    void testDeleteStashedNetworkModifications() throws Exception {
        MvcResult mvcResult;
        EquipmentAttributeModificationInfos loadModificationInfos = EquipmentAttributeModificationInfos.builder()
                .equipmentType(IdentifiableType.LOAD)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentAttributeName("v1load")
                .equipmentId("v1load")
                .build();
        String loadModificationInfosJson = getJsonBody(loadModificationInfos, TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        mvcResult = mockMvc.perform(post(NETWORK_MODIFICATION_URI).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertApplicationStatusOK(mvcResult);

        List<ModificationInfos> modifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modifications.size());
        String uuidString = modifications.get(0).getUuid().toString();
        mockMvc.perform(put(URI_NETWORK_MODIF_BASE)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", uuidString)
                        .queryParam("stashed", "true"))
                .andExpect(status().isOk());
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true, true).size());
        mockMvc.perform(delete("/v1/groups/" + TEST_GROUP_ID + "/stashed-modifications").queryParam("errorOnGroupNotFound", "false")).andExpect(status().isOk());
        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, true, true, true).size());
        mockMvc.perform(delete("/v1/groups/" + UUID.randomUUID() + "/stashed-modifications").queryParam("errorOnGroupNotFound", "false")).andExpect(status().isOk());
    }

    @Test
    void testGetModificationsCount() throws Exception {
        MvcResult mvcResult;
        createSomeSwitchModifications(TEST_GROUP_ID, 3);
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications-count", TEST_GROUP_ID)
                .queryParam("stashed", "false"))
            .andExpect(status().isOk()).andReturn();
        assertEquals(3, Integer.valueOf(mvcResult.getResponse().getContentAsString()).intValue());

        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications-count", TEST_GROUP_ID)
                .queryParam("stashed", "true"))
            .andExpect(status().isOk()).andReturn();
        assertEquals(0, Integer.valueOf(mvcResult.getResponse().getContentAsString()).intValue());

        //Test for stashed parameter default value
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications-count", TEST_GROUP_ID))
            .andExpect(status().isOk()).andReturn();
        assertEquals(3, Integer.valueOf(mvcResult.getResponse().getContentAsString()).intValue());
    }

    @Test
    void testDuplicateCompositeModification() throws Exception {
        // Create a composite modification with the modification
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                .content(mapper.writeValueAsString(modificationUuidList)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Duplicate it without group ownership
        mvcResult = mockMvc.perform(
            post(URI_COMPOSITE_NETWORK_MODIF_BASE + "/duplication")
                    .content(objectWriter.writeValueAsString(List.of(compositeModificationUuid)))
                    .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        Map<UUID, UUID> returnedMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, returnedMap.size());
        Map.Entry<UUID, UUID> returnedIds = returnedMap.entrySet().stream().findFirst().get();
        UUID returnedSourceId = returnedIds.getKey();
        UUID returnedNewId = returnedIds.getValue();
        assertNotEquals(returnedSourceId, returnedNewId);
        assertEquals(compositeModificationUuid, returnedSourceId);

        ModificationInfos sourceModificationInfos = modificationRepository.getModificationInfo(compositeModificationUuid);
        ModificationInfos newModificationInfos = modificationRepository.getModificationInfo(returnedNewId);
        // compare duplicate with the source (same data except uuid)
        assertThat(sourceModificationInfos).recursivelyEquals(newModificationInfos);
        // source group has not changed
        List<ModificationInfos> groupModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, groupModifications.size());
        assertEquals(modificationUuidList.get(0), groupModifications.get(0).getUuid());

        // now delete the duplicate modification
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("uuids", returnedNewId.toString()))
                .andExpect(status().isOk());

        // source group has not changed
        groupModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, groupModifications.size());
        assertEquals(modificationUuidList.get(0), groupModifications.get(0).getUuid());
        // duplicate has been deleted
        assertEquals("MODIFICATION_NOT_FOUND : " + returnedNewId, assertThrows(NetworkModificationException.class, ()
                -> modificationRepository.getModificationInfo(returnedNewId)).getMessage());
    }

    @Test
    void testUpdateNetworkCompositeModification() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 3;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);
        assertEquals(modificationsNumber, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // Create a composite modification with the switch modifications
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Create new modifications to use in the update
        int newModificationsNumber = 2;
        List<ModificationInfos> newModificationList = createSomeSwitchModifications(TEST_GROUP2_ID, newModificationsNumber);
        List<UUID> newModificationUuids = newModificationList.stream().map(ModificationInfos::getUuid).toList();

        // Update the composite modification with the new modifications
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/" + compositeModificationUuid)
                .content(mapper.writeValueAsString(newModificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // Get the composite modification content and verify it has been updated
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + compositeModificationUuid + "/network-modifications?onlyMetadata=false"))
                .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> updatedCompositeContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        assertEquals(newModificationsNumber, updatedCompositeContent.size());
    }

    @Test
    void testUpdateNetworkCompositeModificationWithNonexistentUuid() throws Exception {
        // Try to update a composite modification that doesn't exist
        UUID nonExistentUuid = UUID.randomUUID();
        List<UUID> modificationUuids = List.of(UUID.randomUUID());

        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/" + nonExistentUuid)
                .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void testUpdateNetworkCompositeModificationWithEmptyList() throws Exception {
        // Create a composite modification with some modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 2);
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();

        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Update the composite with an empty list of modifications
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/" + compositeModificationUuid)
                .content(mapper.writeValueAsString(Collections.emptyList())).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // Verify that the composite now contains no modifications
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + compositeModificationUuid + "/network-modifications?onlyMetadata=false"))
                .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> updatedCompositeContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        assertTrue(updatedCompositeContent.isEmpty());
    }

    @Test
    void testMetadata() throws Exception {
        // create a single switch attribute modification in a group
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        UUID switchModificationId = modificationList.get(0).getUuid();

        MvcResult mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_BASE + "/metadata?ids={id}", switchModificationId)
                    .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        List<ModificationMetadata> metadata = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, metadata.size());
        assertEquals(switchModificationId, metadata.get(0).getId());
        assertEquals(EQUIPMENT_ATTRIBUTE_MODIFICATION, metadata.get(0).getType());
    }

    @Test
    void testStandaloneDeletionError() throws Exception {
        // create a single switch attribute modification in a group
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        UUID switchModificationId = modificationList.get(0).getUuid();

        // Try to delete this modification without its group: not allowed
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("uuids", switchModificationId.toString()))
                .andExpectAll(
                    status().is5xxServerError(),
                    content().string(new NetworkModificationException(MODIFICATION_DELETION_ERROR,
                        String.format("%s is owned by group %s", switchModificationId, TEST_GROUP_ID)).getMessage())
            );
    }

    @Test
    void testVerifyModifications() throws Exception {
        // create a single switch attribute modification in a group
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        UUID switchModificationId = modificationList.get(0).getUuid();

        createSomeSwitchModifications(TEST_GROUP2_ID, 1);

        // try to verify unexisting modification
        mockMvc.perform(get("/v1/groups/{groupId}/network-modifications/verify", TEST_GROUP_ID)
            .param("uuids", UUID.randomUUID().toString()))
            .andExpect(status().isNotFound());

        // try to verify invalid modification
        mockMvc.perform(get("/v1/groups/{groupId}/network-modifications/verify", TEST_GROUP2_ID)
                .param("uuids", switchModificationId.toString()))
            .andExpect(status().isNotFound());

        // try to verify valid modification
        mockMvc.perform(get("/v1/groups/{groupId}/network-modifications/verify", TEST_GROUP_ID)
                .param("uuids", switchModificationId.toString()))
            .andExpect(status().isOk());
    }
}
