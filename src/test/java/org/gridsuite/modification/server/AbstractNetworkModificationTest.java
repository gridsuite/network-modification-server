package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
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
import org.springframework.http.*;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/*
Class to extend if you want to test a network modification.
Each modification should have it own class and at least implement the 4 CRUD operations.
 */
@RunWith(SpringRunner.class)
@SpringBootTest(properties = {"spring.data.elasticsearch.enabled=true"})
@AutoConfigureMockMvc
public abstract class AbstractNetworkModificationTest {

    protected static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    protected static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");
    protected static final UUID TEST_GROUP_ID = UUID.randomUUID();
    protected static final UUID TEST_NETWORK_BUS_BREAKER_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    protected static final UUID TEST_NETWORK_MIXED_TOPOLOGY_ID = UUID.fromString("22222222-7977-4592-ba19-88027e4254e4");
    protected static final String VARIANT_NOT_EXISTING_ID = "variant_not_existing";
    protected static final UUID TEST_REPORT_ID = UUID.randomUUID();

    protected static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    protected static final String URI_NETWORK_MODIF_GET_PUT = URI_NETWORK_MODIF_BASE + "/";
    protected static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    protected static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    protected static final String URI_NETWORK_MODIF_BUS_BREAKER = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_BUS_BREAKER_ID + URI_NETWORK_MODIF_PARAMS;
    protected static final String URI_NETWORK_MODIF_MIXED_TOPO = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_MIXED_TOPOLOGY_ID + URI_NETWORK_MODIF_PARAMS;
    protected static final String URI_NETWORK_MODIF_BAD_NETWORK = URI_NETWORK_MODIF_BASE + "?networkUuid=" + NOT_FOUND_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    protected static final String URI_NETWORK_MODIF_BAD_VARIANT = URI_NETWORK_MODIF + "&variantId=" + VARIANT_NOT_EXISTING_ID;

    @Autowired
    protected MockMvc mockMvc;

    @MockBean
    @Qualifier("reportServer")
    protected RestTemplate reportServerRest;

    //    @SpyBean
    @MockBean
    protected NetworkStoreService networkStoreService;

    @Autowired
    protected NetworkModificationService networkModificationService;

    @Autowired
    protected NetworkModificationRepository modificationRepository;

    @Autowired
    protected ObjectMapper mapper;

    protected Network network;
    protected Network networkBusBreaker;
    protected Network networkMixedTopology;

    @Before
    public void setUp() {
        // creating networks
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        networkBusBreaker = NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID);
        networkMixedTopology = NetworkCreation.createMixedTopology(TEST_NETWORK_MIXED_TOPOLOGY_ID);

        // mocking
        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> network);
        when(networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID)).then((Answer<Network>) invocation -> networkBusBreaker);
        when(networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID)).then((Answer<Network>) invocation -> networkMixedTopology);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
                .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // setting service variable
        networkModificationService.setReportServerRest(reportServerRest);
    }

    @After
    public void tearOff() {
        modificationRepository.deleteAll();
    }

    @Test
    public abstract void testCreate() throws Exception;

    @Test
    public abstract void testRead() throws Exception;

    @Test
    public abstract void testUpdate() throws Exception;

    @Test
    public abstract void testDelete() throws Exception;

    protected void testNetworkModificationsCount(UUID groupUuid, int actualSize) throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        // get all modifications for the given group of a network
        mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications?onlyMetadata=true", groupUuid).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> modificationsTestGroupId = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(actualSize, modificationsTestGroupId.size());
    }

}
