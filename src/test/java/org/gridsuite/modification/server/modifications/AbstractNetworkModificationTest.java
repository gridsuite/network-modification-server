/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.WireMockServer;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.WireMockUtils;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.stubbing.Answer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/*
Class to extend if you want to test a network modification.
Each modification should have its own class and implements the abstract methods.
It will automatically run the tests present in this class with the implemented methods.
If you want to add a test that can be applied to every modification, add it here.
If you want to add a test specific to a modification, add it in its own class.
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@DisableElasticsearch
@AutoConfigureMockMvc
public abstract class AbstractNetworkModificationTest {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractNetworkModificationTest.class);

    private static final UUID TEST_NETWORK_ID = UUID.randomUUID();
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.randomUUID();
    protected static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_GET_PUT = URI_NETWORK_MODIF_BASE + "/";
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF_COPY = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY&networkUuid=" + TEST_NETWORK_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID() + "&variantId=" + NetworkCreation.VARIANT_ID;

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

    @Before
    public void setUp() {
        network = createNetwork(TEST_NETWORK_ID);

        modificationRepository.deleteAll();

        initMocks();

        wireMockServer = new WireMockServer(wireMockConfig().dynamicPort());
        wireMockUtils = new WireMockUtils(wireMockServer);
        wireMockServer.start();
    }

    private void initMocks() {
        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> network);
    }

    @After
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

    @Test
    @SneakyThrows
    public void testCreate() {

        ModificationInfos modificationToCreate = buildModification();
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        ModificationInfos createdModification = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(createdModification, createMatcher(modificationToCreate));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        assertNetworkAfterCreation();
    }

    @Test
    @SneakyThrows
    public void testRead() {

        ModificationInfos modificationToRead = buildModification();

        UUID modificationUuid = saveModification(modificationToRead);

        MvcResult mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        ModificationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() {
        });

        assertThat(receivedModification, createMatcher(modificationToRead));
    }

    @Test
    @SneakyThrows
    public void testUpdate() {

        ModificationInfos modificationToUpdate = buildModification();

        UUID modificationUuid = saveModification(modificationToUpdate);

        modificationToUpdate = buildModificationUpdate();

        String modificationToUpdateJson = mapper.writeValueAsString(modificationToUpdate);

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationUuid).content(modificationToUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // TODO Need a test for substations impacted
        //assertThat(bsmListResult.get(0), createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        ModificationInfos updatedModification = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(updatedModification, createMatcher(modificationToUpdate));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    @SneakyThrows
    public void testDelete() {

        ModificationInfos modificationToDelete = buildModification();

        UUID modificationUuid = saveModification(modificationToDelete);

        mockMvc.perform(delete(getNetworkModificationUri())
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", modificationUuid.toString()))
                .andExpect(status().isOk()).andReturn();

        List<ModificationInfos> storedModifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);

        assertTrue(storedModifications.isEmpty());
        assertNetworkAfterDeletion();
    }

    @Test
    @SneakyThrows
    public void testCopy() {

        ModificationInfos modificationToCopy = buildModification();

        UUID modificationUuid = saveModification(modificationToCopy);

        mockMvc.perform(put(URI_NETWORK_MODIF_COPY)
                        .content(mapper.writeValueAsString(List.of(modificationUuid)))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<ModificationInfos> modifications = modificationRepository
                .getModifications(TEST_GROUP_ID, false, true);

        assertEquals(2, modifications.size());
        assertThat(modifications.get(0), createMatcher(modificationToCopy));
        assertThat(modifications.get(1), createMatcher(modificationToCopy));
    }

    @SneakyThrows
    protected void testNetworkModificationsCount(UUID groupUuid, int actualSize) {
        MvcResult mvcResult;
        String resultAsString;
        // get all modifications for the given group of a network
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications?onlyMetadata=true", groupUuid).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> modificationsTestGroupId = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(actualSize, modificationsTestGroupId.size());
    }

    /** Save a network modification into the repository and return its UUID. */
    protected UUID saveModification(ModificationInfos modificationInfos) {
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationInfos.toEntity()));
        return modificationRepository.getModifications(TEST_GROUP_ID, true, true).get(0).getUuid();
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
        return URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    }

    protected String getNetworkModificationUriWithBadVariant() {
        return getNetworkModificationUri() + "&variantId=variant_not_existing";
    }

    protected abstract Network createNetwork(UUID networkUuid);

    protected abstract ModificationInfos buildModification();

    protected abstract ModificationInfos buildModificationUpdate();

    protected abstract MatcherModificationInfos createMatcher(ModificationInfos modificationInfos);

    protected abstract void assertNetworkAfterCreation();

    protected abstract void assertNetworkAfterDeletion();
}
