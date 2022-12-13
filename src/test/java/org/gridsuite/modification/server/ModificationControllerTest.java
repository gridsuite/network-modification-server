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
import nl.jqno.equalsverifier.EqualsVerifier;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.LoadCreationInfos.LoadCreationInfosBuilder;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;
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

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos;
import static org.gridsuite.modification.server.utils.MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos;
import static org.gridsuite.modification.server.utils.MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos;
import static org.gridsuite.modification.server.utils.MatcherLineCreationInfos.createMatcherLineCreationInfos;
import static org.gridsuite.modification.server.utils.MatcherLoadCreationInfos.createMatcherLoadCreationInfos;
import static org.gridsuite.modification.server.utils.MatcherShuntCompensatorCreationInfos.createMatcher;
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
@SpringBootTest(properties = {"spring.data.elasticsearch.enabled=true"})
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
    private static final String URI_NETWORK_MODIF_2 = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID_2 + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_BUS_BREAKER = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_BUS_BREAKER_ID + URI_NETWORK_MODIF_PARAMS;
    private static final String URI_NETWORK_MODIF_MIXED_TOPO = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_MIXED_TOPOLOGY_ID + URI_NETWORK_MODIF_PARAMS;
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
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, new IllegalArgumentException("Error message")).getMessage(), MODIFICATION_ERROR.name() +  " : Error message");
    }

    @Test
    public void testEquipmentIdNonNull() {
        String errorMessage = "equipmentId is marked non-null but is null";
        LoadCreationInfosBuilder<?, ?> loadCreationBuilder = LoadCreationInfos.builder();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationBuilder.build()).getMessage());
        LoadCreationInfosBuilder<?, ?> loadCreationBuilder1 = loadCreationBuilder.equipmentId(null);
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationBuilder1.build()).getMessage());
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().type(ModificationType.LOAD_CREATION).equipmentId("idLoad").build();
        assertEquals(errorMessage, assertThrows(NullPointerException.class, () -> loadCreationInfos.setEquipmentId(null)).getMessage());
    }

    @Test
    public void testEquipmentAttributeModificationInfos() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder()
                .uuid(TEST_NETWORK_ID)
                .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentId("equipmentId")
                .substationIds(Set.of("substationId"))
                .equipmentAttributeName("equipmentAttributeName")
                .equipmentAttributeValue("equipmentAttributeValue")
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
                .build();
        assertEquals("EquipmentAttributeModificationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=7928181c-7977-4592-ba19-88027e4254e4, date=2021-02-19T00:00Z, type=EQUIPMENT_ATTRIBUTE_MODIFICATION, substationIds=[substationId]), equipmentId=equipmentId), equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue, equipmentType=VOLTAGE_LEVEL)", modificationInfos.toString());

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);

        // switch opening
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfosList = mapper.readValue(resultAsString, new TypeReference<>() { });
        EquipmentAttributeModificationInfos modificationSwitchInfos =
                Objects.requireNonNull(equipmentAttributeModificationInfosList).get(0);

        org.hamcrest.MatcherAssert.assertThat(modificationSwitchInfos, MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("v1b1", Set.of("s1"), "open", true, IdentifiableType.SWITCH));

        // switch in variant VARIANT_ID opening
        switchStatusModificationInfos.setEquipmentId("break1Variant");
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF + "&variantId=" + NetworkCreation.VARIANT_ID).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfosListSwitch = mapper.readValue(resultAsString, new TypeReference<>() { });
        modificationSwitchInfos = Objects.requireNonNull(equipmentAttributeModificationInfosListSwitch).get(0);

        org.hamcrest.MatcherAssert.assertThat(modificationSwitchInfos, MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("break1Variant", Set.of("s1Variant"), "open", true, IdentifiableType.SWITCH));
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

    private void switchModifications(String extraParams, String switchId1, String switchNotFoundId, String switchId2, String switchId3,
                                     Set<String> substationsIds, Set<String> otherSubstationsIds,
                                     int modificationsCount) throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentAttributeValue(true)
                .equipmentId(switchId1)
                .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);

        // network not existing
        mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage()));

        // switch not existing
        switchStatusModificationInfos.setEquipmentId(switchNotFoundId);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isNotFound(),
                content().string(new NetworkModificationException(EQUIPMENT_NOT_FOUND, switchNotFoundId).getMessage()));

        // switch closing when already closed
        switchStatusModificationInfos.setEquipmentId(switchId1);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultAttributeModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultAttributeModificationInfos.get(0), createMatcherEquipmentAttributeModificationInfos(switchId1, Set.of(), "open", false, IdentifiableType.SWITCH));

        // switch opening
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultSwitchOpening = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultSwitchOpening.get(0), createMatcherEquipmentAttributeModificationInfos(switchId1, substationsIds, "open", true, IdentifiableType.SWITCH));
        // switch closing
        switchStatusModificationInfos.setEquipmentId(switchId2);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultami = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultami.get(0), createMatcherEquipmentAttributeModificationInfos(switchId2, substationsIds, "open", false, IdentifiableType.SWITCH));

        // switch opening on another substation
        switchStatusModificationInfos.setEquipmentId(switchId3);
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultAttributemi = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultAttributemi.get(0), createMatcherEquipmentAttributeModificationInfos(switchId3, otherSubstationsIds, "open", true, IdentifiableType.SWITCH));

        testNetworkModificationsCount(TEST_GROUP_ID, modificationsCount);
    }

    @Test
    public void testSwitch() throws Exception {
        // switches modifications on initial variant
        switchModifications("", "v1b1", "disc1Variant", "v2b1", "v3b1", Set.of("s1"), Set.of("s2"), 4);

        // switches modifications on variant VARIANT_ID
        switchModifications("&variantId=" + NetworkCreation.VARIANT_ID, "break1Variant", "notFound", "disc1Variant", "break2Variant", Set.of("s1Variant"), Set.of("s2Variant"), 8);
    }

    @Test
    public void testSwitchWithErrors() throws Exception {

        // bad equipment attribute name
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("close") // bad
                .equipmentAttributeValue(true)
                .equipmentId("v1b1")
                .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_NAME_ERROR, "For switch status, the attribute name is only 'open'").getMessage()));

        // bad equipment attribute value
        switchStatusModificationInfos.setEquipmentAttributeName("open");
        switchStatusModificationInfos.setEquipmentAttributeValue("opened"); // bad
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_VALUE_ERROR, "For switch status, the attribute values are only " + Set.of(true, false)).getMessage()));
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
    public void testGroovyWithErrors() throws Exception {

        GroovyScriptModificationInfos groovyScriptModificationInfos = GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("")
                .build();
        String groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);

        // apply empty groovy script
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest());

        groovyScriptModificationInfos.setScript("      ");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        // apply empty groovy script
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage()));

        groovyScriptModificationInfos.setScript("network.getGenerator('there is no generator').targetP=12\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        // apply groovy script with unknown generator
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage()));
    }

    @Test
    public void testGroovy() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        GroovyScriptModificationInfos groovyScriptModificationInfos = GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=12\n")
                .build();
        String groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);

        // apply groovy script with generator target P modification
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrbStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with load type modification
        groovyScriptModificationInfos.setScript("network.getLoad('v1load').loadType=com.powsybl.iidm.network.LoadType.FICTITIOUS\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrBranchStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with lcc converter station power factor modification
        groovyScriptModificationInfos.setScript("network.getLccConverterStation('v1lcc').powerFactor=1\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmListResultBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultBranchStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with line R modification
        groovyScriptModificationInfos.setScript("network.getLine('line1').r=2\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrbInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1", "s2")));

        // apply groovy script with two windings transformer ratio tap modification
        groovyScriptModificationInfos.setScript("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrbsInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbsInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with three windings transformer phase tap modification
        groovyScriptModificationInfos.setScript("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 6);
    }

    @Test
    public void testUndoModificationsOnNetworkFlushError() throws Exception {
        String uriString = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_WITH_FLUSH_ERROR_ID + URI_NETWORK_MODIF_PARAMS;

        GroovyScriptModificationInfos groovyScriptModificationInfos = GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);

        MvcResult mvcResult = mockMvc.perform(post(uriString).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                                      .andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(GROOVY_SCRIPT_ERROR, PowsyblException.class.getName()).getMessage());

        // apply groovy script with 2 modifications with network flush error
        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
    }

    @Test
    public void testMultipleModificationsWithError() throws Exception {
        GroovyScriptModificationInfos groovyScriptModificationInfos = GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .build();
        String groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);

        // apply groovy script without error
        mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // apply groovy script with error ont the second
        groovyScriptModificationInfos.setScript("network.getGenerator('idGenerator').targetP=30\nnetwork.getGenerator('there is no generator').targetP=40\n");
        groovyScriptModificationInfosJson = objectWriter.writeValueAsString(groovyScriptModificationInfos);
        MvcResult mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isBadRequest()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());

        // no modifications have been saved
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
    }

    @Test
    public void testCreateLoadInNodeBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

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
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionName("top")
                .build();

        LoadCreationInfos loadCreationInfos1 = LoadCreationInfos.builder()
                .type(ModificationType.LOAD_CREATION)
                .equipmentId("idLoad2")
                .equipmentName("nameLoad2")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .loadType(LoadType.AUXILIARY)
                .activePower(100.0)
                .reactivePower(60.0)
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();

        String loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmListResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResult.get(0), createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        assertNotNull(network.getLoad("idLoad1"));  // load was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database
        // create load without connection name and UNDEFINED direction
        String loadCreationInfosJson1 = objectWriter.writeValueAsString(loadCreationInfos1);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson1).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isOk()).andReturn();
        // create load with errors
        mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().isNotFound(), content().string(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage())).andReturn();

        loadCreationInfos.setEquipmentId("");
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_LOAD_ERROR, "Invalid id ''").getMessage())).andReturn();

        loadCreationInfos.setEquipmentId("idLoad1");
        loadCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage())).andReturn();

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "Bus bar section notFoundBusbarSection not found").getMessage())).andReturn();

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setActivePower(Double.NaN);
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_LOAD_ERROR, "Load 'idLoad1': p0 is invalid").getMessage())).andReturn();

        loadCreationInfos1.setConnectionDirection(null);
        loadCreationInfos1.setConnectionName(null);
        loadCreationInfosJson1 = objectWriter.writeValueAsString(loadCreationInfos1);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationInfosJson1).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_LOAD_ERROR, "java.lang.NullPointerException").getMessage())).andReturn();

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        // Test create load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be created
        loadCreationInfos.setEquipmentId("idLoad3");
        loadCreationInfos.setEquipmentName("nameLoad3");
        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setConnectionName("cn3");
        loadCreationInfos.setConnectionDirection(ConnectablePosition.Direction.BOTTOM);

        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrModification = mapper.readValue(resultAsString, new TypeReference<>() { });

        assertTrue(bsmlrModification.isEmpty());  // no modifications returned
        assertNull(network.getLoad("idLoad3"));  // load was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 3);  // new modification stored in the database

    }

    @Test
    public void testCreateLoadInBusBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new load in voltage level with bus/breaker topology (in voltage level "VLGEN" and bus "NGEN")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
                .type(ModificationType.LOAD_CREATION)
                .equipmentId("idLoad1")
                .equipmentName("nameLoad1")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .loadType(LoadType.FICTITIOUS)
                .activePower(200.0)
                .reactivePower(30.0)
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();

        LoadCreationInfos loadCreationInfos1 = LoadCreationInfos.builder()
                .type(ModificationType.LOAD_CREATION)
                .equipmentId("idLoad2")
                .equipmentName("nameLoad2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .loadType(LoadType.FICTITIOUS)
                .activePower(200.0)
                .reactivePower(30.0)
                .build();

        String loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(loadCreationInfosJson)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrEquipmentModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        EquipmentModificationInfos equipmentModificationInfos = bsmlrEquipmentModification.get(0);
        assertThat(equipmentModificationInfos, createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        var listModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true);

        // create load without connection name and direction
        String loadCreationInfosJson1 = objectWriter.writeValueAsString(loadCreationInfos1);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(loadCreationInfosJson1)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        // Update load creation
        loadCreationInfos = new LoadCreationEntity(
                "idLoad1edited",
                "nameLoad1edited",
                LoadType.AUXILIARY,
                "v12",
                "bus12",
                175.0,
                60.0, "top", ConnectablePosition.Direction.TOP, 0)
                .toModificationInfos();
        loadCreationInfos.setUuid(listModifications.get(0).getUuid());
        loadCreationInfos.setType(ModificationType.LOAD_CREATION);

        LoadCreationInfos loadCreationUpdate = new LoadCreationEntity(
                "idLoad1edited",
                "nameLoad1edited",
                LoadType.AUXILIARY,
                "v12",
                "bus12",
                175.0,
                60.0, "bot", ConnectablePosition.Direction.BOTTOM, 1)
                .toModificationInfos();
        loadCreationUpdate.setType(ModificationType.LOAD_CREATION);
        String loadCreationUpdateJson = objectWriter.writeValueAsString(loadCreationUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + listModifications.get(0).getUuid()).content(loadCreationUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 2);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + listModifications.get(0).getUuid()))
                .andExpectAll(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        LoadCreationInfos bsmlrLoadCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrLoadCreation, createMatcherLoadCreationInfos(loadCreationInfos));

        // create load with errors
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage()));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);
    }

    @Test
    public void assertThrowsUpdateLoadCreation() {
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
                .type(ModificationType.LOAD_CREATION)
                .equipmentId("idLoad1")
                .equipmentName("nameLoad1")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .loadType(LoadType.FICTITIOUS)
                .activePower(200.0)
                .reactivePower(30.0)
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();

        UUID modificationUuid = UUID.randomUUID();

        String errorMessage = assertThrows(NetworkModificationException.class, () -> networkModificationService.updateModification(modificationUuid, loadCreationInfos)).getMessage();
        assertEquals(new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)).getMessage(), errorMessage);

        assertThrows(NullPointerException.class, () -> networkModificationService.updateModification(modificationUuid, null));
    }

    @Test
    public void testModifyLoad() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        EqualsVerifier.simple().forClass(AttributeModification.class).verify();

        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .type(ModificationType.LOAD_MODIFICATION)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(LoadType.AUXILIARY, OperationType.SET))
                .activePower(new AttributeModification<>(100.0, OperationType.SET))
                .build();

        String loadModificationInfosJson = objectWriter.writeValueAsString(loadModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlreModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlreModificationInfos.get(0), createMatcherEquipmentModificationInfos(ModificationType.LOAD_MODIFICATION, "v1load", Set.of("s1")));

        assertNotNull(network.getLoad("v1load"));  // load was modified
        assertEquals(LoadType.AUXILIARY, network.getLoad("v1load").getLoadType());
        assertEquals(100.0, network.getLoad("v1load").getP0(), 0.1);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database

        // modify load with errors
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        loadModificationInfos.setEquipmentId("unknownLoadId");
        loadModificationInfosJson = objectWriter.writeValueAsString(loadModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrModificationInfos.get(0), createMatcherEquipmentModificationInfos(ModificationType.LOAD_MODIFICATION, "unknownLoadId", Set.of()));
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // Modify all attributes of the load
        loadModificationInfos = LoadModificationInfos.builder()
                .type(ModificationType.LOAD_MODIFICATION)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1Load", OperationType.SET))
                .activePower(new AttributeModification<>(80.0, OperationType.SET))
                .reactivePower(new AttributeModification<>(40.0, OperationType.SET))
                .voltageLevelId(new AttributeModification<>("newVlId", OperationType.SET))
                .busOrBusbarSectionId(new AttributeModification<>("newBusbarId", OperationType.SET))
                .build();

        loadModificationInfosJson = objectWriter.writeValueAsString(loadModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlreModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlreModification.get(0), createMatcherEquipmentModificationInfos(ModificationType.LOAD_MODIFICATION, "v1load", Set.of("s1")));

        assertNotNull(network.getLoad("v1load"));  // load was modified
        var equipment = network.getLoad("v1load");
        assertEquals("newV1Load", network.getLoad("v1load").getNameOrId());
        assertEquals(LoadType.FICTITIOUS, equipment.getLoadType());
        assertEquals(80.0, equipment.getP0(), 0.1);
        assertEquals(40.0, equipment.getQ0(), 0.1);
        // TODO check connectivity when it will be implemented
        testNetworkModificationsCount(TEST_GROUP_ID, 3);  // new modification stored in the database

        // Unset an attribute that should not be null
        loadModificationInfos = LoadModificationInfos.builder()
                .type(ModificationType.LOAD_MODIFICATION)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        loadModificationInfosJson = objectWriter.writeValueAsString(loadModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFY_LOAD_ERROR, "Load 'v1load': load type is null").getMessage());

        // Update load modification
        loadModificationInfos = new LoadModificationEntity(
                "v1load",
                null,
                new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET),
                null,
                null,
                null,
                new AttributeModification<>(70.0, OperationType.SET))
                .toModificationInfos();
        loadModificationInfos.setUuid(bsmlreModification.get(0).getUuid());

        LoadModificationInfos loadModificationUpdate = LoadModificationInfos.builder()
                .type(ModificationType.LOAD_MODIFICATION)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
                .reactivePower(new AttributeModification<>(70.0, OperationType.SET))
                .build();
        String loadModificationUpdateJson = objectWriter.writeValueAsString(loadModificationUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlreModification.get(0).getUuid()).content(loadModificationUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlreModification.get(0).getUuid()))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        LoadModificationInfos bsmlrloadmodif = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrloadmodif, MatcherLoadModificationInfos.createMatcherLoadModificationInfos(loadModificationInfos));
    }

    @Test
    public void testModifyGenerator() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        String generatorId = "idGenerator";
        GeneratorModificationInfos generatorModificationInfos = GeneratorModificationInfos.builder()
            .type(ModificationType.GENERATOR_MODIFICATION)
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
            .maxActivePower(new AttributeModification<>(100.0, OperationType.SET))
            .build();

        String generatorModificationInfosJson = objectWriter.writeValueAsString(generatorModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmListResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResult.get(0), createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, generatorId, Set.of("s1")));

        assertNotNull(network.getGenerator(generatorId));  // generator was modified
        assertEquals(EnergySource.HYDRO, network.getGenerator(generatorId).getEnergySource());
        assertEquals(100.0, network.getGenerator(generatorId).getMaxP(), 0.1);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database

        // modify generator with errors
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        String anotherId = "unknownGeneratorId";
        generatorModificationInfos.setEquipmentId(anotherId);
        generatorModificationInfosJson = objectWriter.writeValueAsString(generatorModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrGeneratoModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrGeneratoModification.get(0), createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, anotherId, Set.of()));
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // Modify all attributes of the generator
        generatorModificationInfos = GeneratorModificationInfos.builder()
            .type(ModificationType.GENERATOR_MODIFICATION)
            .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
            .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
            .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
            .voltageSetpoint(new AttributeModification<>(48.0, OperationType.SET))
            .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
            .minActivePower(new AttributeModification<>(0., OperationType.SET))
            .maxActivePower(new AttributeModification<>(100., OperationType.SET))
            .ratedNominalPower(new AttributeModification<>(220., OperationType.SET))
            .equipmentId(generatorId)
            .build();

        generatorModificationInfosJson = objectWriter.writeValueAsString(generatorModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).contentType(MediaType.APPLICATION_JSON).content(generatorModificationInfosJson))
                                         .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrGeneModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrGeneModification.get(0), createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, generatorId, Set.of("s1")));

        assertNotNull(network.getGenerator(generatorId));  // generator was modified
        var equipment = network.getGenerator(generatorId);
        assertEquals("newV1Generator", equipment.getNameOrId());
        assertEquals(EnergySource.SOLAR, equipment.getEnergySource());
        assertEquals(80.0, equipment.getTargetP(), .1);
        assertEquals(40.0, equipment.getTargetQ(), .1);
        assertEquals(48.0, equipment.getTargetV(), .1);
        assertTrue(equipment.isVoltageRegulatorOn());
        assertEquals(0.0, equipment.getMinP(), .1);
        assertEquals(100.0, equipment.getMaxP(), .1);
        assertEquals(220.0, equipment.getRatedS(), .1);

        // TODO check connectivity when it will be implemented
        testNetworkModificationsCount(TEST_GROUP_ID, 3);  // new modification stored in the database

        // Unset an attribute that should not be null
        generatorModificationInfos = GeneratorModificationInfos.builder()
            .type(ModificationType.GENERATOR_MODIFICATION)
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(null, OperationType.UNSET))
            .build();

        generatorModificationInfosJson = objectWriter.writeValueAsString(generatorModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
           .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Generator '" + generatorId + "': energy source is not set").getMessage());

    }

    @Test
    public void testUpdateModifyGenerator() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        String generatorId = "idGenerator";
        GeneratorModificationInfos generatorModificationInfos = GeneratorModificationInfos.builder()
            .type(ModificationType.GENERATOR_MODIFICATION)
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
            .maxActivePower(new AttributeModification<>(100.0, OperationType.SET))
            .build();

        String generatorModificationInfosJson = objectWriter.writeValueAsString(generatorModificationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrGroupId = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrGroupId.get(0), createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, generatorId, Set.of("s1")));

        var listModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, listModifications.size());

        generatorModificationInfos = GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
            .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
            .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
            .voltageSetpoint(new AttributeModification<>(48.0, OperationType.SET))
            .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
            .minActivePower(new AttributeModification<>(0., OperationType.SET))
            .maxActivePower(new AttributeModification<>(100., OperationType.SET))
            .ratedNominalPower(new AttributeModification<>(220., OperationType.SET))
            .uuid(listModifications.get(0).getUuid())
            .type(ModificationType.GENERATOR_MODIFICATION)
            .build();
        generatorModificationInfosJson = objectWriter.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + listModifications.get(0).getUuid()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        var modifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);

        assertEquals(1, modifications.size());
        modifications.get(0).setDate(listModifications.get(0).getDate()); // this one is modified by sql database
        assertEquals(generatorModificationInfos, modifications.get(0));
    }

    @Test
    public void testCreateShuntCompensator() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        ShuntCompensatorCreationInfos shunt1 = ShuntCompensatorCreationInfos.builder()
            .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
            .equipmentId("shuntOneId").equipmentName("hop")
            .currentNumberOfSections(4).maximumNumberOfSections(9)
            .susceptancePerSection(1.).isIdenticalSection(true)
            .voltageLevelId("v2").busOrBusbarSectionId("1B")
            .connectionName("cn1")
            .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
            .build();

        String shunt1Json = objectWriter.writeValueAsString(shunt1);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(shunt1Json).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrShuntCompensator = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrShuntCompensator.get(0), createMatcherEquipmentModificationInfos(ModificationType.SHUNT_COMPENSATOR_CREATION, "shuntOneId", Set.of("s1")));

        assertNotNull(network.getShuntCompensator("shuntOneId"));  // shunt compensator was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        shunt1 = new ShuntCompensatorCreationEntity(shunt1).toModificationInfos();
        shunt1.setEquipmentId("shuntOneIdEdited");
        shunt1.setEquipmentName("hopEdited");
        shunt1.setIsIdenticalSection(false);
        shunt1.setCurrentNumberOfSections(6);
        shunt1.setMaximumNumberOfSections(12);
        shunt1.setSusceptancePerSection(2.);
        shunt1.setVoltageLevelId("v4");
        shunt1.setBusOrBusbarSectionId("1.A");
        shunt1.setUuid(bsmlrShuntCompensator.get(0).getUuid());

        // Update shunt compansator creation
        ShuntCompensatorCreationInfos shuntUpdate = new ShuntCompensatorCreationEntity(shunt1).toModificationInfos();
        shuntUpdate.setEquipmentId("shuntOneIdEdited");
        shuntUpdate.setEquipmentName("hopEdited");
        shuntUpdate.setIsIdenticalSection(false);
        shuntUpdate.setCurrentNumberOfSections(6);
        shuntUpdate.setMaximumNumberOfSections(12);
        shuntUpdate.setSusceptancePerSection(2.);
        shuntUpdate.setVoltageLevelId("v4");
        shuntUpdate.setBusOrBusbarSectionId("1.A");
        String shuntUpdateJson = objectWriter.writeValueAsString(shuntUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlrShuntCompensator.get(0).getUuid()).content(shuntUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlrShuntCompensator.get(0).getUuid()).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        ShuntCompensatorCreationInfos bsmListResultGetModifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultGetModifications, createMatcher(shunt1));

        shunt1.setMaximumNumberOfSections(2);
        shunt1Json = objectWriter.writeValueAsString(shunt1);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(shunt1Json).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError());
    }

    @Test
    public void testCreateShuntCompensatorInBusBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        ShuntCompensatorCreationInfos shunt1 = ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .equipmentId("shuntTwoId").equipmentName("Shunt")
                .currentNumberOfSections(4).maximumNumberOfSections(9)
                .susceptancePerSection(1.).isIdenticalSection(true)
                .voltageLevelId("v2").busOrBusbarSectionId("bus2")
                .connectionName("cn2")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();

        String shunt1Json = objectWriter.writeValueAsString(shunt1);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shunt1Json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrShuntCompensator = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals("shuntTwoId", bsmlrShuntCompensator.get(0).getEquipmentId());
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        shunt1 = new ShuntCompensatorCreationEntity(shunt1).toModificationInfos();
        shunt1.setEquipmentId("shuntTwoIdEdited");
        shunt1.setEquipmentName("shuntEdited");
        shunt1.setIsIdenticalSection(false);
        shunt1.setCurrentNumberOfSections(6);
        shunt1.setMaximumNumberOfSections(12);
        shunt1.setSusceptancePerSection(2.);
        shunt1.setVoltageLevelId("v1");
        shunt1.setBusOrBusbarSectionId("bus1");
        shunt1.setUuid(bsmlrShuntCompensator.get(0).getUuid());

        // Update shunt compansator creation
        ShuntCompensatorCreationInfos shuntUpdate = new ShuntCompensatorCreationEntity(shunt1).toModificationInfos();
        shuntUpdate.setEquipmentId("shuntTwoIdEdited");
        shuntUpdate.setEquipmentName("shuntEdited");
        shuntUpdate.setIsIdenticalSection(false);
        shuntUpdate.setCurrentNumberOfSections(6);
        shuntUpdate.setMaximumNumberOfSections(12);
        shuntUpdate.setSusceptancePerSection(2.);
        shuntUpdate.setVoltageLevelId("v1");
        shuntUpdate.setBusOrBusbarSectionId("bus1");
        String shuntUpdateJson = objectWriter.writeValueAsString(shuntUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlrShuntCompensator.get(0).getUuid()).content(shuntUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlrShuntCompensator.get(0).getUuid()))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        ShuntCompensatorCreationInfos bsmListResultGetModifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultGetModifications, createMatcher(shunt1));

        shunt1.setMaximumNumberOfSections(2);
        shunt1Json = objectWriter.writeValueAsString(shunt1);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shunt1Json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // create shunt compensator with errors
        shunt1.setBusOrBusbarSectionId("notFoundBus");
        shunt1Json = objectWriter.writeValueAsString(shunt1);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shunt1Json).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage()));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);
    }

    @Test
    public void testCreateGeneratorInNodeBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new generator in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v2", "idGenerator1", "idGenerator1", "1B", "v2load", "LOAD", "v1");
        String generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrGeneratorCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrGeneratorCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_CREATION, "idGenerator1", Set.of("s1")));

        assertNotNull(network.getGenerator("idGenerator1"));  // generator was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        generatorCreationInfos = new GeneratorCreationEntity(
                "idGenerator1Edited",
                "nameGenerator1Edited",
                EnergySource.SOLAR,
                "v1",
                "1A",
                150.,
                300.,
                15.,
                450.,
                55.,
                false,
                235.,
                80.,
                30.,
                300.,
                true,
                10f,
                25.,
                44.,
                List.of(new ReactiveCapabilityCurveCreationEmbeddable(45., 85., 77.)),
                "v2load",
                "LOAD",
                "v2",
                25.,
                false,
                "top1",
                ConnectablePosition.Direction.TOP,
                20)
                .toModificationInfos();
        generatorCreationInfos.setUuid(bsmlrGeneratorCreation.get(0).getUuid());

        // Update generator creation
        GeneratorCreationInfos generatorCreationUpdate = new GeneratorCreationEntity(
                "idGenerator1Edited",
                "nameGenerator1Edited",
                EnergySource.SOLAR,
                "v1",
                "1A",
                150.,
                300.,
                15.,
                450.,
                55.,
                false,
                235.,
                80.,
                30.,
                300.,
                true,
                10f,
                25.,
                44.,
                List.of(new ReactiveCapabilityCurveCreationEmbeddable(45., 85., 77.)),
                "v2load",
                "LOAD",
                "v2",
                25.,
                false,
                "top11",
                ConnectablePosition.Direction.TOP,
                21)
                .toModificationInfos();
        String generatorCreationUpdateJson = objectWriter.writeValueAsString(generatorCreationUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlrGeneratorCreation.get(0).getUuid()).content(generatorCreationUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlrGeneratorCreation.get(0).getUuid()).contentType(MediaType.APPLICATION_JSON))
                 .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        GeneratorCreationInfos bsmlrCreationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrCreationInfos, createMatcherGeneratorCreationInfos(generatorCreationInfos));

        // create generator with errors
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        generatorCreationInfos.setEquipmentId("");
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_GENERATOR_ERROR, "Invalid id ''").getMessage());

        generatorCreationInfos.setEquipmentId("idGenerator1");
        generatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage());

        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_GENERATOR_ERROR, "Busbar section notFoundBusbarSection not found.").getMessage());
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfos.setMinActivePower(Double.NaN);
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1': invalid value (NaN) for minimum P").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create generator on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the generator cannot be created
        generatorCreationInfos.setEquipmentId("idGenerator3");
        generatorCreationInfos.setEquipmentName("nameGenerator3");
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getGenerator("idGenerator3"));  // generator was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateGeneratorInBusBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new generator in voltage level with bus/breaker topology
        GeneratorCreationInfos generatorCreationInfos = ModificationCreation.getCreationGenerator("v1", "idGenerator2", "nameGenerator2", "bus1", "idGenerator1", "GENERATOR", "v1");
        String generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrCreationGenerator = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrCreationGenerator.get(0), createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_CREATION, "idGenerator2", Set.of("s1")));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create generator with errors
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        generatorCreationInfosJson = objectWriter.writeValueAsString(generatorCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
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
                null
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
                null
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

    @Test
    public void testDeleteEquipment() throws Exception {
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

        // update equipment deletion
        equipmentDeletionInfos.setEquipmentType("GENERATOR");
        equipmentDeletionInfos.setEquipmentId("idGenerator");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlrEquipmentDeletion.get(0).getUuid()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlrEquipmentDeletion.get(0).getUuid()))
                .andExpectAll(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        EquipmentDeletionInfos createdEquipmentDeletion = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(createdEquipmentDeletion, createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "idGenerator", "GENERATOR", Set.of()));

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

        // delete equipment with errors
        equipmentDeletionInfos.setEquipmentType("LOAD");
        equipmentDeletionInfos.setEquipmentId("v1load");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        equipmentDeletionInfos.setEquipmentType("LOAD");
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=notFoundLoad not found or of bad type").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

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
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV5 = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV5.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v5", "VOLTAGE_LEVEL", Set.of("s3")));

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

        // delete voltage level (fail because the vl is connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        assertThrows("\"status\":500,\"error\":\"Internal Server Error\",\"message\":\"The voltage level 'v4' cannot be removed because of a remaining THREE_WINDINGS_TRANSFORMER",
                NestedServletException.class, () -> mockMvc.perform(post(URI_NETWORK_MODIF).content(objectWriter.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON)));
        assertNotNull(network.getVoltageLevel("v4"));

        // delete substation
        equipmentDeletionInfos.setEquipmentType("SUBSTATION");
        equipmentDeletionInfos.setEquipmentId("s3");
        equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsS3 = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsS3.get(0), createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "s3", "SUBSTATION", Set.of("s3")));

        testNetworkModificationsCount(TEST_GROUP_ID, 15);

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

        // delete substation (fail because the substations is connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        assertThrows("DELETE_EQUIPMENT_ERROR : The substation s2 is still connected to another substation", NestedServletException.class, () ->
                mockMvc.perform(post(URI_NETWORK_MODIF).content(objectWriter.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON)).andReturn());
        assertNotNull(network.getSubstation("s2"));

        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());
        assertEquals(55, equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).size());
        assertEquals(6, equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID_2).size());
    }

    @Test
    public void testOkWhenRemovingIsolatedEquipment() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v5load")
                .build();
        String equipmentDeletionInfosJson = objectWriter.writeValueAsString(equipmentDeletionInfos);

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        mockMvc.perform(post(URI_NETWORK_MODIF).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        var v5 = network.getVoltageLevel("v5");
        assertNull(v5.getNodeBreakerView().getTerminal(2));
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
            put("/v1/groups/" + TEST_GROUP_ID + "?action=COPY")
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
                put("/v1/groups/" + otherGroupId + "?action=COPY")
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
    public void testMoveModification() throws Exception {
        // create 2 modifications
        List<UUID> modificationUuidList = createSomeSwitchModifications(TEST_GROUP_ID, 2).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());

        // swap modifications: move [1] before [0]
        List<UUID> movingModificationUuidList = Collections.singletonList(modificationUuidList.get(1));
        mockMvc.perform(
            put("/v1/groups/" + TEST_GROUP_ID + "?action=MOVE&originGroupUuid=" + TEST_GROUP_ID + "&before=" + modificationUuidList.get(0))
                    .content(objectWriter.writeValueAsString(movingModificationUuidList))
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        var newModificationUuidList = modificationRepository.getModifications(TEST_GROUP_ID, true, true).
                stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
        assertNotNull(newModificationUuidList);
        Collections.reverse(newModificationUuidList);

        assertEquals(modificationUuidList, newModificationUuidList);
    }

    @Test
    public void testCreateLineInNodeBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "1.1" and
        //         voltage level "v2" and busbar section "1.1"
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
            .type(ModificationType.LINE_CREATION)
            .equipmentId("idLine4")
            .equipmentName("nameLine4")
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
            .connectionName1("cn1Line4")
            .connectionDirection1(ConnectablePosition.Direction.TOP)
            .connectionName2("cn2Line4")
            .connectionDirection2(ConnectablePosition.Direction.TOP)
            .build();
        String lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);

        assertEquals("LineCreationInfos(super=BranchCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, date=null, type=LINE_CREATION, substationIds=[]), equipmentId=idLine4), equipmentName=nameLine4), seriesResistance=100.0, seriesReactance=100.0, voltageLevelId1=v1, voltageLevelId2=v2, busOrBusbarSectionId1=1.1, busOrBusbarSectionId2=1A, currentLimits1=null, currentLimits2=null, connectionName1=cn1Line4, connectionDirection1=TOP, connectionName2=cn2Line4, connectionDirection2=TOP), shuntConductance1=10.0, shuntSusceptance1=10.0, shuntConductance2=20.0, shuntSusceptance2=20.0)", lineCreationInfos.toString());
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrLineCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrLineCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine4", Set.of("s1")));
        assertNotNull(network.getLine("idLine4"));  // line was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfos = new LineCreationEntity(
                "idLine4edited",
                "nameLine4edited",
                110.0,
                110.0,
                15.0,
                15.,
                25.,
                25.,
                "v2",
                "1A",
                "v1",
                "1.1",
                5.,
                5.,
                "cn13",
                ConnectablePosition.Direction.TOP,
                "cn23",
                ConnectablePosition.Direction.BOTTOM)
                .toModificationInfos();
        lineCreationInfos.setUuid(bsmlrLineCreation.get(0).getUuid());
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);

        // Update load creation
        LineCreationInfos lineCreationUpdate = new LineCreationEntity(
                "idLine4edited",
                "nameLine4edited",
                110.0,
                110.0,
                15.0,
                15.,
                25.,
                25.,
                "v2",
                "1A",
                "v1",
                "1.1",
                5.,
                5.,
                "cn14",
                ConnectablePosition.Direction.TOP,
                "cn24",
                ConnectablePosition.Direction.BOTTOM).toModificationInfos();
        String lineCreationUpdateJson = objectWriter.writeValueAsString(lineCreationUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + bsmlrLineCreation.get(0).getUuid()).content(lineCreationUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + bsmlrLineCreation.get(0).getUuid()))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        LineCreationInfos bsmlrLineCreationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrLineCreationInfos, createMatcherLineCreationInfos(lineCreationInfos));

        // create line with errors
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        lineCreationInfos.setEquipmentId("");
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "Invalid id ''").getMessage());

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setSeriesResistance(Double.NaN);
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': r is invalid").getMessage());

        lineCreationInfos.setSeriesResistance(100.0);
        lineCreationInfos.setSeriesReactance(Double.NaN);
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': x is invalid").getMessage());
        lineCreationInfos.setSeriesReactance(100.0);

        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        lineCreationInfos.setEquipmentId("idLine5");
        lineCreationInfos.setEquipmentName("nameLine5");
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertNotNull(modifications);

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getLine("idLine5"));  // line was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateLineInBusBreaker() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
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
            .build();

        String lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modifications.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create line with errors
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
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
    public void replaceTeePointByVoltageLevelOnLineDuplicateModificationGroupTest() throws Exception  {
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
    public void testCreateLineInMixedTypology() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new line in voltage levels with node breaker topology and bus breaker topology
        // between voltage level "v1" and busbar section "1.1" type NODE_BREAKER and
        //         voltage level "v2" and busbar section "bus2 type BUS_BREAKER"
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
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .connectionName1("cn1Line1")
            .connectionDirection1(ConnectablePosition.Direction.TOP)
            .connectionName2("cn2Line1")
            .connectionDirection2(ConnectablePosition.Direction.TOP)
            .build();

        String lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_MIXED_TOPO).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modifications.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        //create line with errors
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_MIXED_TOPO).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setBusOrBusbarSectionId2("notFoundBus");
        lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_MIXED_TOPO).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new line without shunt conductance or reactance
        LineCreationInfos lineCreationInfosNoShunt = LineCreationInfos.builder()
            .type(ModificationType.LINE_CREATION)
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .seriesResistance(100.0)
            .seriesReactance(100.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .build();

        String lineCreationInfosNoShuntJson = objectWriter.writeValueAsString(lineCreationInfosNoShunt);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosNoShuntJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modificationsLineCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modificationsLineCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfosNoShunt.setShuntConductance1(50.0);
        lineCreationInfosNoShunt.setShuntConductance2(null);
        lineCreationInfosNoShunt.setShuntSusceptance1(null);
        lineCreationInfosNoShunt.setShuntSusceptance2(60.0);

        lineCreationInfosNoShuntJson = objectWriter.writeValueAsString(lineCreationInfosNoShunt);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosNoShuntJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modificationListEquipment = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modificationListEquipment.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        LineCreationInfos lineCreationInfosPermanentLimitOK = LineCreationInfos.builder()
            .type(ModificationType.LINE_CREATION)
            .equipmentId("idLine2")
            .equipmentName("nameLine2")
            .seriesResistance(100.0)
            .seriesReactance(100.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(1.0).build())
            .build();

        String lineCreationInfosPermanentLimitOKJson = objectWriter.writeValueAsString(lineCreationInfosPermanentLimitOK);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosPermanentLimitOKJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modificationlEquipment = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modificationlEquipment.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine2", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        lineCreationInfosPermanentLimitOK.setCurrentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).build());
        lineCreationInfosPermanentLimitOK.setCurrentLimits2(CurrentLimitsInfos.builder().permanentLimit(null).build());
        lineCreationInfosPermanentLimitOKJson = objectWriter.writeValueAsString(lineCreationInfosPermanentLimitOK);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosPermanentLimitOKJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modificationsModificationLineCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modificationsModificationLineCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine2", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        LineCreationInfos lineCreationInfosPermanentLimitNOK = LineCreationInfos.builder()
            .type(ModificationType.LINE_CREATION)
            .equipmentId("idLine2")
            .equipmentName("nameLine2")
            .seriesResistance(100.0)
            .seriesReactance(100.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(0.0).build())
            .build();
        String lineCreationInfosPermanentLimitNOKJson = objectWriter.writeValueAsString(lineCreationInfosPermanentLimitNOK);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosPermanentLimitNOKJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine2': permanent limit must be defined and be > 0").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        LineCreationInfos lineCreationInfosOK = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId("idLine3")
                .equipmentName("nameLine3")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(1.0).build())
                .build();

        String lineCreationInfosJson = objectWriter.writeValueAsString(lineCreationInfosOK);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    public void testCreateSubstation() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        // create new substation
        SubstationCreationInfos substationCreationInfos = SubstationCreationInfos.builder()
                .type(ModificationType.SUBSTATION_CREATION)
                .equipmentId("SubstationId")
                .equipmentName("SubstationName")
                .substationCountry(Country.AF)
                .build();
        String substationCreationInfosJson = objectWriter.writeValueAsString(substationCreationInfos);
        assertEquals("SubstationCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, date=null, type=SUBSTATION_CREATION, substationIds=[]), equipmentId=SubstationId), equipmentName=SubstationName), substationCountry=AF)", substationCreationInfos.toString());
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(substationCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modificationsSubstationCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(modificationsSubstationCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.SUBSTATION_CREATION, "SubstationId", Set.of("SubstationId")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        substationCreationInfos = new SubstationCreationEntity(
                "SubstationIdEdited",
                "SubstationNameEdited",
                Country.CI)
                .toModificationInfos();
        substationCreationInfos.setUuid(modificationsSubstationCreation.get(0).getUuid());
        substationCreationInfosJson = objectWriter.writeValueAsString(substationCreationInfos);

        // Update substation creation
        SubstationCreationInfos substationCreationUpdate = new SubstationCreationEntity(
                "SubstationIdEdited",
                "SubstationNameEdited",
                Country.CI).toModificationInfos();
        String substationCreationUpdateJson = objectWriter.writeValueAsString(substationCreationUpdate);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationsSubstationCreation.get(0).getUuid()).content(substationCreationUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andReturn();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + modificationsSubstationCreation.get(0).getUuid()))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        SubstationCreationInfos listModificationsSubstationCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(listModificationsSubstationCreation, MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(substationCreationInfos));

        // create substation with errors
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(substationCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        substationCreationInfos.setEquipmentId("");
        substationCreationInfosJson = objectWriter.writeValueAsString(substationCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(substationCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Invalid id ''").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateVoltageLevel() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        VoltageLevelCreationInfos vli = ModificationCreation.getCreationVoltageLevel("absent_station", "vlId", "vlName");
        String vliJson = objectWriter.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(vliJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(SUBSTATION_NOT_FOUND, "absent_station").getMessage());

        vli = ModificationCreation.getCreationVoltageLevel("s1", "vlId", "vlName");
        vli.getBusbarConnections().add(BusbarConnectionCreationInfos.builder().fromBBS("bbs.ne").toBBS("bbs.ne").switchKind(SwitchKind.DISCONNECTOR).build());
        String vliJsonObject = objectWriter.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Disconnector between same bus bar section 'bbs.ne'").getMessage());

        // then success
        vli = ModificationCreation.getCreationVoltageLevel("s1", "vlId", "vlName");
        String vliJsonS1Object = objectWriter.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(vliJsonS1Object).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> listModificationsVoltageLevelCreation = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(listModificationsVoltageLevelCreation.get(0), createMatcherEquipmentModificationInfos(ModificationType.VOLTAGE_LEVEL_CREATION, "vlId", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        vli = new VoltageLevelCreationEntity(
                "VoltageLevelIdEdited",
                "VoltageLevelEdited",
                385.,
                "s2",
                List.of(),
                List.of())
                .toModificationInfos();
        vli.setUuid(listModificationsVoltageLevelCreation.get(0).getUuid());
        vli.setType(ModificationType.VOLTAGE_LEVEL_CREATION);
        String vliJsonS2Object = objectWriter.writeValueAsString(vli);

        // Update voltage level creation
        VoltageLevelCreationInfos vlu = new VoltageLevelCreationEntity(
                "VoltageLevelIdEdited",
                "VoltageLevelEdited",
                385.,
                "s2",
                List.of(),
                List.of())
                .toModificationInfos();
        vlu.setType(ModificationType.VOLTAGE_LEVEL_CREATION);
        String vluInfosJson = objectWriter.writeValueAsString(vlu);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + listModificationsVoltageLevelCreation.get(0).getUuid()).content(vluInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + listModificationsVoltageLevelCreation.get(0).getUuid()).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        VoltageLevelCreationInfos listModificationsCreatMatcher = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(listModificationsCreatMatcher, MatcherVoltageLevelCreationInfos.createMatcherVoltageLevelCreationInfos(vli));

        // create substation with errors
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(vliJsonS2Object).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        vli.setEquipmentId("");
        vliJsonS2Object = objectWriter.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(vliJsonS2Object).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Invalid id ''").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
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
    public void testLineSplitWithVoltageLevel() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        VoltageLevelCreationInfos vl1 = VoltageLevelCreationInfos.builder()
                .type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .equipmentId("vl1")
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
                .busbarConnections(List.of())
                .build();

        LineSplitWithVoltageLevelInfos lineSplitAbsentLine = LineSplitWithVoltageLevelInfos.builder()
                .type(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL)
                .lineToSplitId("absent_line_id")
                .percent(10.0)
                .mayNewVoltageLevelInfos(vl1)
                .existingVoltageLevelId(null)
                .bbsOrBusId("v1bbs")
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();

        String lineSplitAbsentLineJson = objectWriter.writeValueAsString(lineSplitAbsentLine);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineSplitAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage());

        LineSplitWithVoltageLevelInfos lineSplitWoVL = LineSplitWithVoltageLevelInfos.builder()
                .type(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL)
                .lineToSplitId("line3")
                .percent(10.0)
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("v4")
                .bbsOrBusId("1.A")
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
        String lineSplitWoVLJson = objectWriter.writeValueAsString(lineSplitWoVL);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(lineSplitWoVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineSplitWithVoltageLevelInfos lineSplitWithNewVL = LineSplitWithVoltageLevelInfos.builder()
                .type(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL)
                .lineToSplitId("line2")
                .percent(10.0)
                .mayNewVoltageLevelInfos(vl1)
                .existingVoltageLevelId(null)
                .bbsOrBusId("v1bbs")
                .newLine1Id("nl1v")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2v")
                .newLine2Name("NewLine2")
                .build();
        String lineSplitWithNewVLJson = objectWriter.writeValueAsString(lineSplitWithNewVL);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineSplitWithNewVLJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> result = mapper.readValue(resultAsString, new TypeReference<>() { });

        assertNotNull(result);
        Optional<ModificationInfos> lineSplitProper = result.stream().filter(r -> r.getType() == ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL).findFirst();
        assertTrue(lineSplitProper.isPresent());
        assertEquals(2, lineSplitProper.get().getSubstationIds().size());

        Optional<ModificationInfos> lineDeletion = result.stream().filter(r -> r.getType() == ModificationType.EQUIPMENT_DELETION).findFirst();
        assertTrue(lineDeletion.isPresent());
        // EquipmentDeletionInfos is erased down to a EquipmentModificationInfos by ->json->
        //   but handled ok in StudyServer where we added equipementType to EquipementModificationInfos.
        //assertTrue(lineDeletion.get() instanceof EquipmentDeletionInfos);
        //assertEquals("LINE", ((EquipmentDeletionInfos)lineDeletion.get()).getEquipmentType());

        var modifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true)
            .stream().map(LineSplitWithVoltageLevelInfos.class::cast).collect(Collectors.toList());
        assertEquals(2, modifications.size());
        assertTrue(modifications.stream().filter(r -> r.getLineToSplitId().equals("line2")).findFirst().isPresent());
        assertTrue(modifications.stream().filter(r -> r.getLineToSplitId().equals("line3")).findFirst().isPresent());
        var modification = modifications.stream().filter(r -> r.getLineToSplitId().equals("line2")).findFirst().get();
        lineSplitWithNewVL.setPercent(20.0);
        String lineSplitWithNewVLUpdJson = objectWriter.writeValueAsString(lineSplitWithNewVL);
        UUID uuidNotFound = UUID.randomUUID();
        mvcResult = mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + uuidNotFound).content(lineSplitWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", uuidNotFound)).getMessage());
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modification.getUuid()).content(lineSplitWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
    }

    @Test
    public void testLineAttachToVoltageLevel() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        VoltageLevelCreationInfos vl1 = VoltageLevelCreationInfos.builder()
                .type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .equipmentId("vl1")
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
                .busbarConnections(List.of())
                .build();

        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId("attachmentLine")
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();

        LineAttachToVoltageLevelInfos lineAttachToAbsentLine = new LineAttachToVoltageLevelInfos("absent_line_id",
                10.0, "AttPointId", "attPointName", vl1, null,
                "v1bbs", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");
        lineAttachToAbsentLine.setType(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);
        String lineAttachToAbsentLineJson = objectWriter.writeValueAsString(lineAttachToAbsentLine);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage());

        LineAttachToVoltageLevelInfos lineAttachToVL = new LineAttachToVoltageLevelInfos("line3",
                10.0, "AttPointId", "attPointName", null, "v4",
                "1.A", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");
        lineAttachToVL.setType(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);

        String lineAttachToVLJson = objectWriter.writeValueAsString(lineAttachToVL);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(lineAttachToVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        LineAttachToVoltageLevelInfos lineAttachToWithNewVL = new LineAttachToVoltageLevelInfos("line3",
                10.0, "AttPointId", "attPointName", vl1, null,
                "1.A", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");
        lineAttachToWithNewVL.setType(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);
        String lineAttachToWithNewVLJson = objectWriter.writeValueAsString(lineAttachToWithNewVL);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineAttachToWithNewVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> result = mapper.readValue(resultAsString, new TypeReference<>() { });
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        assertNotNull(result);
        Optional<ModificationInfos> lineAttachToProper = result.stream().filter(r -> r.getType() == ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL).findFirst();
        assertTrue(lineAttachToProper.isPresent());
        assertEquals(3, lineAttachToProper.get().getSubstationIds().size());
        assertTrue(lineAttachToProper.get().getSubstationIds().contains("s1"));
        assertTrue(lineAttachToProper.get().getSubstationIds().contains("s2"));
        assertTrue(lineAttachToProper.get().getSubstationIds().contains("AttPointId_substation"));

        Optional<ModificationInfos> lineDeletion = result.stream().filter(r -> r.getType() == ModificationType.EQUIPMENT_DELETION).findFirst();
        assertTrue(lineDeletion.isPresent());
        assertEquals("line3", ((EquipmentModificationInfos) lineDeletion.get()).getEquipmentId());

        LineAttachToVoltageLevelInfos incompleteLineAttachToVL = new LineAttachToVoltageLevelInfos("line3",
                10.0, "AttPointId", "attPointName", null, "v4",
                "1.A", null, "nl1", "NewLine1", "nl2", "NewLine2");
        incompleteLineAttachToVL.setType(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);

        String incompleteLineAttachToVLJson = objectWriter.writeValueAsString(incompleteLineAttachToVL);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(incompleteLineAttachToVLJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(LINE_ATTACH_ERROR, "Missing required attachment line description").getMessage());
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        LineAttachToVoltageLevelInfos lineAttachWithNewVLUpd = new LineAttachToVoltageLevelInfos("line2",
                10.0, "AttPointId", "attPointName", vl1, null,
                "1.A", attachmentLine, "nl1", "NewLine1", "nl2", "NewLine2");
        lineAttachWithNewVLUpd.setType(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);
        String lineAttachWithNewVLUpdJson = objectWriter.writeValueAsString(lineAttachWithNewVLUpd);
        mvcResult = mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + new UUID(128, 16)).content(lineAttachWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(LINE_ATTACH_NOT_FOUND, "Line attach not found").getMessage());
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + lineAttachToProper.get().getUuid()).content(lineAttachWithNewVLUpdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

    }

    @Test
    public void testAttachLinesToSplitLines() throws Exception {
        assertNotNull(networkWithTeePoint.getLine("l2"));
        assertNotNull(networkWithTeePoint.getLine("l3"));
        assertEquals(3, networkWithTeePoint.getLineCount());
        assertNotNull(networkWithTeePoint.getVoltageLevel("v2"));
        assertNotNull(networkWithTeePoint.getVoltageLevel("v4"));
        assertEquals(4, networkWithTeePoint.getVoltageLevelCount());

        LinesAttachToSplitLinesInfos linesAttachToAbsentLine1 = new LinesAttachToSplitLinesInfos("absent_line_id", "l2", "l3", "v4", "bbs2", "nl1", "NewLine1", "nl2", "NewLine2");
        linesAttachToAbsentLine1.setType(ModificationType.LINES_ATTACH_TO_SPLIT_LINES);
        String linesAttachToAbsentLine1Json = objectWriter.writeValueAsString(linesAttachToAbsentLine1);

        MvcResult mvcResult = mockMvc.perform(post(URI_NETWORK_WITH_TEE_POINT_MODIF).content(linesAttachToAbsentLine1Json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals("LINE_NOT_FOUND : Line absent_line_id is not found", resultAsString);

        // fix to have to correct modification
        linesAttachToAbsentLine1.setLineToAttachTo1Id("l1");
        String linesAttachToSplitLinesJson = objectWriter.writeValueAsString(linesAttachToAbsentLine1);
        mvcResult = mockMvc.perform(post(URI_NETWORK_WITH_TEE_POINT_MODIF).content(linesAttachToSplitLinesJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();

        List<ModificationInfos> result = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertNotNull(result);
        assertEquals(18, result.size()); // FIXME why ??? we get a result for this modification and all individual deletion
        ModificationInfos linesAttachToProperSplitLines = result.stream().filter(r -> r.getType() == ModificationType.LINES_ATTACH_TO_SPLIT_LINES).findFirst().orElseThrow();
        assertEquals(Set.of("s3", "s4", "s1", "s2"), linesAttachToProperSplitLines.getSubstationIds());
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        assertNull(networkWithTeePoint.getLine("l1"));
        assertNull(networkWithTeePoint.getLine("l2"));
        assertNotNull(networkWithTeePoint.getLine("nl1"));
        assertNotNull(networkWithTeePoint.getLine("nl2"));
        assertNull(networkWithTeePoint.getLine("l3"));
        assertEquals(2, networkWithTeePoint.getLineCount());
        assertNull(networkWithTeePoint.getVoltageLevel("v2"));
        assertNotNull(networkWithTeePoint.getVoltageLevel("v4"));
        assertEquals(3, networkWithTeePoint.getVoltageLevelCount());

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + linesAttachToProperSplitLines.getUuid()).content(linesAttachToSplitLinesJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
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
