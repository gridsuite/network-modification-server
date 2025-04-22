/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.WireMockUtils;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Class to extend if you want to test a network modification.<ul>
 * <li>Each modification should have its own class and implements the abstract methods.</li>
 * <li>It will automatically run the tests present in this class with the implemented methods.</li>
 * <li>If you want to add a test that can be applied to every modification, add it here.</li>
 * <li>If you want to add a test specific to a modification, add it in its own class.</li>
 * </ul>
 */
@SpringBootTest
@DisableElasticsearch
@AutoConfigureMockMvc
public abstract class AbstractNetworkModificationTest {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractNetworkModificationTest.class);

    private static final UUID TEST_NETWORK_ID = UUID.randomUUID();
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_GET_PUT = URI_NETWORK_MODIF_BASE + "/";
    private static final String URI_NETWORK_MODIF_COPY = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY";

    @Autowired
    protected MockMvc mockMvc;

    protected WireMockServer wireMockServer;

    protected WireMockUtils wireMockUtils;

    @MockBean
    private NetworkStoreService networkStoreService;

    @MockBean
    protected ReportService reportService;

    @Autowired
    protected NetworkModificationRepository modificationRepository;

    @Autowired
    protected ObjectMapper mapper;

    private Network network;

    @BeforeEach
    public void setUp() {
        network = createNetwork(TEST_NETWORK_ID);

        modificationRepository.deleteAll();

        when(networkStoreService.getNetwork(eq(NOT_FOUND_NETWORK_ID), any(PreloadingStrategy.class))).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), any(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);

        wireMockServer = new WireMockServer(wireMockConfig().dynamicPort());
        wireMockUtils = new WireMockUtils(wireMockServer);
        wireMockServer.start();
    }

    @AfterEach
    public void tearOff() {
        modificationRepository.deleteAll();

        try {
            TestUtils.assertWiremockServerRequestsEmptyThenShutdown(wireMockServer);
        } catch (UncheckedInterruptedException e) {
            LOGGER.error("Error while attempting to get the request done : ", e);
        } catch (IOException e) {
            // Ignoring
        }
    }

    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
    }

    @Test
    public void testCreate() throws Exception {
        MvcResult mvcResult;
        NetworkModificationsResult networkModificationsResult;
        ModificationInfos modificationToCreate = buildModification();
        String bodyJson = getJsonBody(modificationToCreate, null);

        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(bodyJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());
        assertResultImpacts(getNetworkImpacts(networkModificationsResult));
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, extractApplicationStatus(networkModificationsResult).getFirst());
        ModificationInfos createdModification = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(createdModification).recursivelyEquals(modificationToCreate);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        assertAfterNetworkModificationCreation();

        ModificationInfos createdModificationWithOnlyMetadata = modificationRepository.getModifications(TEST_GROUP_ID, true, true).get(0);
        testCreationModificationMessage(createdModificationWithOnlyMetadata);
    }

    @Test
    public void testCreateDisabledModification() throws Exception {
        MvcResult mvcResult;
        NetworkModificationsResult networkModificationsResult;
        ModificationInfos modificationToCreate = buildModification();
        modificationToCreate.setActivated(false);
        String modificationToCreateJson = getJsonBody(modificationToCreate, null);

        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());

        assertEquals(0, getNetworkImpacts(networkModificationsResult).size());
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, extractApplicationStatus(networkModificationsResult).getFirst());
        ModificationInfos createdModification = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(createdModification).recursivelyEquals(modificationToCreate);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        // when modification is not active, element created by the modifications should NOT be present in network
        assertAfterNetworkModificationDeletion();

        ModificationInfos createdModificationWithOnlyMetadata = modificationRepository.getModifications(TEST_GROUP_ID, true, true).get(0);
        testCreationModificationMessage(createdModificationWithOnlyMetadata);
        assertEquals(false, createdModificationWithOnlyMetadata.getActivated());
    }

    @Test
    public void testRead() throws Exception {
        MvcResult mvcResult;
        NetworkModificationResult networkModificationResult;
        ModificationInfos modificationToRead = buildModification();

        UUID modificationUuid = saveModification(modificationToRead);

        mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, networkModificationResult.getApplicationStatus());
        String resultAsString = mvcResult.getResponse().getContentAsString();
        ModificationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(receivedModification).recursivelyEquals(modificationToRead);
    }

    @Test
    public void testUpdate() throws Exception {
        ModificationInfos modificationToUpdate = buildModification();

        UUID modificationUuid = saveModification(modificationToUpdate);

        modificationToUpdate = buildModificationUpdate();

        String modificationToUpdateJson = mapper.writeValueAsString(modificationToUpdate);

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationUuid).content(modificationToUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // TODO Need a test for substations impacted
        //assertThat(bsmListResult.get(0)).recursivelyEquals(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1"));

        ModificationInfos updatedModification = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(updatedModification).recursivelyEquals(modificationToUpdate);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        ModificationInfos updatedModificationwithOnlyMetadata = modificationRepository.getModifications(TEST_GROUP_ID, true, true).get(0);
        testUpdateModificationMessage(updatedModificationwithOnlyMetadata);

    }

    @Test
    public void testDelete() throws Exception {
        ModificationInfos modificationToDelete = buildModification();

        UUID modificationUuid = saveModification(modificationToDelete);

        mockMvc.perform(delete(getNetworkModificationUri())
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", modificationUuid.toString()))
                .andExpect(status().isOk());

        List<ModificationInfos> storedModifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);

        assertTrue(storedModifications.isEmpty());
        assertAfterNetworkModificationDeletion();
    }

    @Test
    public void testCopy() throws Exception {
        ModificationInfos modificationToCopy = buildModification();

        UUID modificationUuid = saveModification(modificationToCopy);
        String body = TestUtils.getJsonBody(List.of(modificationUuid), AbstractNetworkModificationTest.TEST_NETWORK_ID, null);
        mockMvc.perform(put(URI_NETWORK_MODIF_COPY)
                        .content(body)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        List<ModificationInfos> modifications = modificationRepository
                .getModifications(TEST_GROUP_ID, false, true);

        assertEquals(2, modifications.size());
        assertThat(modifications.get(0)).recursivelyEquals(modificationToCopy);
        assertThat(modifications.get(1)).recursivelyEquals(modificationToCopy);
    }

    protected void testNetworkModificationsCount(UUID groupUuid, int actualSize) throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        // get all modifications for the given group of a network
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true", groupUuid).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> modificationsTestGroupId = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(actualSize, modificationsTestGroupId.size());
    }

    /** Save a network modification into the repository and return its UUID. */
    protected UUID saveModification(ModificationInfos modificationInfos) {
        ModificationEntity entity = ModificationEntity.fromDTO(modificationInfos);
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(entity));
        return entity.getId();
    }

    protected Network getNetwork() {
        return network;
    }

    protected void setNetwork(Network network) {
        this.network = network;
    }

    protected UUID getNetworkId() {
        return TEST_NETWORK_ID;
    }

    protected UUID getNetworkUuid() {
        return ((NetworkImpl) network).getUuid();
    }

    protected UUID getGroupId() {
        return TEST_GROUP_ID;
    }

    protected String getNetworkModificationUri() {
        return URI_NETWORK_MODIF_BASE + "?groupUuid=" + TEST_GROUP_ID;
    }

    protected String getJsonBody(ModificationInfos modificationInfos, String variantId) throws JsonProcessingException {
        return TestUtils.getJsonBody(modificationInfos, AbstractNetworkModificationTest.TEST_NETWORK_ID, variantId);
    }

    protected abstract Network createNetwork(UUID networkUuid);

    protected abstract ModificationInfos buildModification();

    protected abstract ModificationInfos buildModificationUpdate();

    protected abstract void assertAfterNetworkModificationCreation();

    protected abstract void assertAfterNetworkModificationDeletion();

    @SuppressWarnings("java:S1130") // Exceptions are throws by overrides
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("{}", modificationInfos.getMessageValues());
    }

    @SuppressWarnings("java:S1130") // Exceptions are throws by overrides
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("{}", modificationInfos.getMessageValues());
    }

    protected List<NetworkModificationResult.ApplicationStatus> extractApplicationStatus(NetworkModificationsResult networkModificationsResult) {
        List<NetworkModificationResult.ApplicationStatus> applicationStatuses = new ArrayList<>();
        networkModificationsResult.modificationResults().forEach(modificationResult -> {
            modificationResult.ifPresent(networkModificationResult -> applicationStatuses.add(networkModificationResult.getApplicationStatus()));
        });
        return Optional.of(applicationStatuses).orElse(List.of());
    }

    protected List<AbstractBaseImpact> getNetworkImpacts(NetworkModificationsResult networkModificationsResult) {
        return networkModificationsResult.modificationResults().stream()
                .filter(Optional::isPresent)
                .map(Optional::get)
                .flatMap(result -> result.getNetworkImpacts().stream())
                .toList();
    }

    protected Set<String> getImpactedSubstationsIds(NetworkModificationsResult networkModificationsResult) {
        return networkModificationsResult.modificationResults().stream()
                .filter(Optional::isPresent)
                .map(Optional::get)
                .flatMap(result -> result.getImpactedSubstationsIds().stream())
                .collect(Collectors.toSet());
    }
}
