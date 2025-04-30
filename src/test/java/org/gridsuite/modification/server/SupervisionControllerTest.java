package org.gridsuite.modification.server;

import org.apache.http.HttpHost;
import org.elasticsearch.client.RestClient;
import org.gridsuite.modification.server.service.SupervisionService;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.web.servlet.MockMvc;

import java.util.UUID;

import static org.mockito.Mockito.times;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@SpringBootTest
@DisableElasticsearch
class SupervisionControllerTest {
    @SpyBean
    private SupervisionService supervisionService;

    @Autowired
    MockMvc mockMvc;

    @Autowired
    private RestClient restClient;

    @Test
    void testGetElasticSearchHost() throws Exception {
        HttpHost httpHost = restClient.getNodes().get(0).getHost();
        String host = httpHost.getHostName()
            + ":"
            + httpHost.getPort();

        mockMvc.perform(get("/v1/supervision/elasticsearch-host"))
            .andExpectAll(
                status().isOk(),
                content().string(host)
            );
    }

    @Test
    void testReindexAllModifications() throws Exception {
        UUID networkUuid = UUID.randomUUID();
        Mockito.doNothing().when(supervisionService).reindexByNetworkUuid(networkUuid);
        mockMvc.perform(post("/v1/supervision/network-modifications/reindex").param("networkUuid", networkUuid.toString()))
            .andExpectAll(
                status().isOk()
            );

        Mockito.verify(supervisionService, times(1)).reindexByNetworkUuid(networkUuid);
    }

    @Test
    void testRecreateModificationsIndex() throws Exception {
        Mockito.doNothing().when(supervisionService).recreateIndex();
        mockMvc.perform(post("/v1/supervision/network-modifications/index"))
            .andExpectAll(
                status().isOk()
            );

        Mockito.verify(supervisionService, times(1)).recreateIndex();
    }

    @Test
    void testIndexationCount() throws Exception {
        Mockito.when(supervisionService.getIndexModificationsCount()).thenReturn(15L);
        mockMvc.perform(get("/v1/supervision/network-modifications/indexation-count"))
            .andExpectAll(
                status().isOk(),
                content().string("15")
            );

        Mockito.verify(supervisionService, times(1)).getIndexModificationsCount();
    }

    @Test
    void testToReindexCount() throws Exception {
        Mockito.when(supervisionService.getModificationsToReindexCount()).thenReturn(20L);
        mockMvc.perform(get("/v1/supervision/network-modifications/to-reindex-count"))
            .andExpectAll(
                status().isOk(),
                content().string("20")
            );

        Mockito.verify(supervisionService, times(1)).getModificationsToReindexCount();
    }

    @Test
    void testGetIndexName() throws Exception {
        mockMvc.perform(get("/v1/supervision/network-modifications/index-name"))
            .andExpectAll(
                status().isOk(),
                content().string("modifications")
            );
    }
}
