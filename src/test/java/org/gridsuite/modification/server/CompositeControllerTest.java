/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.util.Pair;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.ModificationType.COMPOSITE_MODIFICATION;
import static org.gridsuite.modification.server.utils.NetworkCreation.VARIANT_ID;
import static org.gridsuite.modification.server.utils.TestUtils.runRequestAsync;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@AutoConfigureMockMvc
@SpringBootTest
class CompositeControllerTest {
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP2_ID = UUID.randomUUID();
    private static final String URI_COMPOSITE_NETWORK_MODIF_BASE = "/v1/network-composite-modifications";
    private static final String URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT = "/v1/network-composite-modifications/";
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";

    @Autowired
    private MockMvc mockMvc;

    private ObjectWriter objectWriter;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @MockitoBean
    private ReportService reportService;

    @Autowired
    private ObjectMapper mapper;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    private Network network;

    @BeforeEach
    void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);
        modificationRepository.deleteAll();
    }

    @AfterEach
    void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
    }

    @Test
    void testSplit() throws Exception {
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

        List<ModificationInfos> modificationInfosList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(modificationsNumber, modificationInfosList.size());

        // get the composite modification (metadata only)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> compositeModificationContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(modificationsNumber, compositeModificationContent.size());
        for (int i = 0; i < modificationUuids.size(); i++) {
            assertEquals(modificationInfosList.get(i).getMessageValues(), compositeModificationContent.get(i).getMessageValues());
        }
        assertNotNull(compositeModificationContent.getFirst().getMessageType());
        assertNotNull(compositeModificationContent.getFirst().getMessageValues());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeName());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeValue());

        // create another composite modification
        List<ModificationInfos> otherModificationList = createSomeSwitchModifications(TEST_GROUP2_ID, modificationsNumber);
        List<UUID> otherModificationUuids = otherModificationList.stream().map(ModificationInfos::getUuid).toList();
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(otherModificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        ModificationInfos otherCompositeModificationInfos = CompositeModificationInfos.builder()
                .modifications(otherModificationList)
                .build();
        UUID otherCompositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(modificationRepository.getModificationInfo(otherCompositeModificationUuid)).recursivelyEquals(otherCompositeModificationInfos);

        // get both composite modifications
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids=" + compositeModificationUuid + "&uuids=" + otherCompositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> compositeModificationsContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(modificationsNumber * 2, compositeModificationsContent.size());

        // get the composite modification (complete data)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}&onlyMetadata=false", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        compositeModificationContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        checkCompositeModificationContent(compositeModificationContent);

        // Insert the composite modification in the group
        final String bodyJson = getJsonBodyModificationCompositeInfos(
                List.of(Pair.of(compositeModificationUuid, "random name")));
        mvcResult = runRequestAsync(
                mockMvc,
                put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/" + TEST_GROUP_ID + "?action=SPLIT")
                        .content(bodyJson)
                        .contentType(MediaType.APPLICATION_JSON),
                status().isOk());

        assertApplicationStatusOK(mvcResult);

        List<ModificationInfos> newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(modificationsNumber * 2, newModificationList.size());
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).toList();
        assertEquals(modificationUuids.getFirst(), newModificationUuidList.getFirst());
        assertThat(modificationList.getFirst()).recursivelyEquals(newModificationList.get(modificationsNumber));
    }

    @Test
    void testInsert() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 2;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);

        // Create a composite modification with the switch modification
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        List<ModificationInfos> modificationInfosList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(modificationsNumber, modificationInfosList.size());

        // Insert the composite modification in the group
        final String bodyJson = getJsonBodyModificationCompositeInfos(
                List.of(Pair.of(compositeModificationUuid, "random name")));

        // insert the same composite modification inside as a complete composite, not split into regular network modifications
        mvcResult = runRequestAsync(
                mockMvc,
                put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/" + TEST_GROUP_ID + "?action=INSERT")
                        .content(bodyJson).contentType(MediaType.APPLICATION_JSON), status().isOk()
        );
        assertApplicationStatusOK(mvcResult);
        List<ModificationInfos> newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(modificationsNumber + 1, newModificationList.size());
        CompositeModificationInfos insertedComposite = (CompositeModificationInfos) newModificationList.stream().filter(modificationInfos ->
                modificationInfos.getType().equals(COMPOSITE_MODIFICATION)).findFirst().orElseThrow();
        assertNotNull(insertedComposite);
        checkCompositeModificationContent(insertedComposite.getModifications());
    }

    private static void checkCompositeModificationContent(List<ModificationInfos> compositeModificationContent) {
        assertEquals("open", ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeName());
        assertEquals(Boolean.TRUE, ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeValue());
        assertEquals(IdentifiableType.SWITCH, ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentType());
        assertEquals("v1b1", ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentId());
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
            String bodyJson = TestUtils.getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, VARIANT_ID);
            mvcResult = runRequestAsync(mockMvc, post(URI_NETWORK_MODIF_BASE + "?groupUuid=" + groupId).content(bodyJson).contentType(MediaType.APPLICATION_JSON), status().isOk());
            assertApplicationStatusOK(mvcResult);
        }
        var modificationList = modificationRepository.getModifications(groupId, false, true);
        assertEquals(number, modificationList.size());
        return modificationList;
    }

    private String getJsonBodyModificationCompositeInfos(List<Pair<UUID, String>> modifs) throws JsonProcessingException {
        return TestUtils.getJsonBodyModificationCompositeInfos(modifs, TEST_NETWORK_ID, VARIANT_ID);
    }

    private void assertApplicationStatusOK(MvcResult mvcResult) throws Exception {
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().getFirst().isPresent());
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, networkModificationsResult.modificationResults().getFirst().get().getApplicationStatus());
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
        assertEquals(modificationUuidList.getFirst(), groupModifications.getFirst().getUuid());

        // now delete the duplicate modification
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("uuids", returnedNewId.toString()))
                .andExpect(status().isOk());

        // source group has not changed
        groupModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, groupModifications.size());
        assertEquals(modificationUuidList.getFirst(), groupModifications.getFirst().getUuid());
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
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}&onlyMetadata=false", compositeModificationUuid))
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
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}&onlyMetadata=false", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        List<ModificationInfos> updatedCompositeContent = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        assertTrue(updatedCompositeContent.isEmpty());
    }
}
