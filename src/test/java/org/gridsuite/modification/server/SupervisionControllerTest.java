/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.HttpHost;
import org.elasticsearch.client.RestClient;
import org.gridsuite.modification.server.dto.catalog.AerialLineTypeInfos;
import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;
import org.gridsuite.modification.server.dto.catalog.UndergroundLineTypeInfos;
import org.gridsuite.modification.server.service.LineTypesCatalogService;
import org.gridsuite.modification.server.service.SupervisionService;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMultipartHttpServletRequestBuilder;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.times;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@SpringBootTest
@DisableElasticsearch
class SupervisionControllerTest {

    private static final String SUPERVISION_URI_LINE_CATALOG = "/v1/supervision/network-modifications/catalog/line_types";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_1 = "/lines-catalog.json.gz";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_2 = "/line_types_catalog_2.json.gz";
    private static final String LINE_TYPES_CATALOG_JSON_FILE_3 = "/line_types_catalog_3.json.gz";
    private static final String NOT_EXISTING_JSON_FILE = "/not_existing_file.json.gz";

    @MockitoSpyBean
    private SupervisionService supervisionService;

    @Autowired
    private LineTypesCatalogService lineTypesCatalogService;

    @Autowired
    MockMvc mockMvc;

    @Autowired
    private RestClient restClient;

    @Autowired
    private ObjectMapper mapper;

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

    @Test
    void testGetLineTypesCatalog() throws Exception {
        // Check if the catalog is empty
        List<LineTypeInfos> emptyLineTypes = lineTypesCatalogService.getAllLineTypes();
        assertEquals(0, emptyLineTypes.size());

        // Create the catalog with some line types
        mockMvc.perform(multipart(SUPERVISION_URI_LINE_CATALOG)
                        .file(createMockMultipartFile(LINE_TYPES_CATALOG_JSON_FILE_1)))
                .andExpect(status().isOk());

        // Check if the catalog is complete avoiding the duplicate entry
        List<LineTypeInfos> lineTypes = lineTypesCatalogService.getAllLineTypes();
        assertEquals(10, lineTypes.size());

        // need to delete before adding the new catalog
        mockMvc.perform(delete(SUPERVISION_URI_LINE_CATALOG))
                .andExpect(status().isOk());

        // Check if catalog is completely updated
        mockMvc.perform(multipart(SUPERVISION_URI_LINE_CATALOG)
                        .file(createMockMultipartFile(LINE_TYPES_CATALOG_JSON_FILE_2)))
                .andExpect(status().isOk());

        List<LineTypeInfos> lineTypes2 = lineTypesCatalogService.getAllLineTypes();
        assertEquals(2, lineTypes2.size());

        mockMvc.perform(delete(SUPERVISION_URI_LINE_CATALOG))
                .andExpect(status().isOk());

        // Check if the catalog is empty
        emptyLineTypes = lineTypesCatalogService.getAllLineTypes();
        assertEquals(0, emptyLineTypes.size());
    }

    @Test
    void testGetLineTypeWithLimitsCatalog() throws Exception {
        // Create the catalog with some line types
        mockMvc.perform(multipart(SUPERVISION_URI_LINE_CATALOG)
                        .file(createMockMultipartFile(LINE_TYPES_CATALOG_JSON_FILE_3)))
                .andExpect(status().isOk());

        List<LineTypeInfos> lineTypes = lineTypesCatalogService.getAllLineTypes();
        assertEquals(2, lineTypes.size());
        UUID aerialLineId = lineTypes.get(0) instanceof AerialLineTypeInfos ? lineTypes.get(0).getId() : lineTypes.get(1).getId();
        UUID underGroundLineId = lineTypes.get(0) instanceof UndergroundLineTypeInfos ? lineTypes.get(0).getId() : lineTypes.get(1).getId();

        // get one aerial line with limits
        LineTypeInfos selectedLineType = lineTypesCatalogService.getLineTypesWithLimits(aerialLineId, "1", "37", null);

        assertEquals(1, selectedLineType.getLimitsForLineType().size());
        assertEquals("LimitSet1", selectedLineType.getLimitsForLineType().getFirst().getLimitSetName());
        assertEquals(10.0, selectedLineType.getLimitsForLineType().getFirst().getPermanentLimit());
        assertEquals(20.0, selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits().getFirst().getLimitValue());
        assertEquals("TemporaryLimit1", selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits().getFirst().getName());
        assertEquals(100, selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits().getFirst().getAcceptableDuration());
        assertEquals("37", selectedLineType.getLimitsForLineType().getFirst().getTemperature());
        assertEquals("1", selectedLineType.getLimitsForLineType().getFirst().getArea());

        // get one underground line with limits
        selectedLineType = lineTypesCatalogService.getLineTypesWithLimits(underGroundLineId, "1", null, "0.9");
        assertEquals(1, selectedLineType.getLimitsForLineType().size());
        assertEquals("LimitSet1", selectedLineType.getLimitsForLineType().getFirst().getLimitSetName());
        assertEquals(11.0, selectedLineType.getLimitsForLineType().getFirst().getPermanentLimit(), 0.01);
        assertEquals(22.0, selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits().getFirst().getLimitValue(), 0.01);
        assertEquals("TemporaryLimit1", selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits().getFirst().getName());
        assertEquals(100, selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits().getFirst().getAcceptableDuration());
        assertEquals("37", selectedLineType.getLimitsForLineType().getFirst().getTemperature());
        assertEquals("1", selectedLineType.getLimitsForLineType().getFirst().getArea());

        mockMvc.perform(delete(SUPERVISION_URI_LINE_CATALOG))
                .andExpect(status().isOk());
    }

    @Test
    void testPostLineTypeWithLimitsCatalogError() throws Exception {
        MockMultipartHttpServletRequestBuilder mockMultipartHttpServletRequestBuilder = multipart(SUPERVISION_URI_LINE_CATALOG)
                .file(createMockMultipartFile(NOT_EXISTING_JSON_FILE));

        mockMvc.perform(mockMultipartHttpServletRequestBuilder)
                .andExpect(result -> {
                    assertNotNull(result.getResolvedException());
                    assertEquals("java.io.EOFException",
                            result.getResolvedException().getMessage());
                });
    }

    @Test
    void testGetLineTypeWithoutLimitsCatalog() throws Exception {
        // Check if the catalog is empty
        List<LineTypeInfos> emptyLineTypes = lineTypesCatalogService.getAllLineTypes();
        assertEquals(0, emptyLineTypes.size());

        // Create the catalog with some line types
        mockMvc.perform(multipart(SUPERVISION_URI_LINE_CATALOG)
                        .file(createMockMultipartFile(LINE_TYPES_CATALOG_JSON_FILE_3)))
                .andExpect(status().isOk());

        List<LineTypeInfos> lineTypes = lineTypesCatalogService.getAllLineTypes();
        assertEquals(2, lineTypes.size());
        // getting the whole catalog does not load the limits
        assertNull(lineTypes.get(0).getLimitsForLineType());
        assertNull(lineTypes.get(1).getLimitsForLineType());

        // get one line of the catalog does not load limits too
        LineTypeInfos selectedLineType = lineTypesCatalogService.getLineTypesWithAreaTemperatureShapeFactors(lineTypes.get(0).getId());
        assertEquals(2, selectedLineType.getLimitsForLineType().size());
        assertNull(selectedLineType.getLimitsForLineType().getFirst().getLimitSetName());
        assertNull(selectedLineType.getLimitsForLineType().getFirst().getPermanentLimit());
        assertNull(selectedLineType.getLimitsForLineType().getFirst().getTemporaryLimits());
        assertEquals("1", selectedLineType.getLimitsForLineType().getFirst().getArea());
        assertEquals("37", selectedLineType.getLimitsForLineType().getFirst().getTemperature());

        mockMvc.perform(delete(SUPERVISION_URI_LINE_CATALOG))
                .andExpect(status().isOk());
    }

    private MockMultipartFile createMockMultipartFile(String fileName) throws IOException {
        try (InputStream inputStream = getClass().getResourceAsStream(fileName)) {
            return new MockMultipartFile("file", fileName, MediaType.MULTIPART_FORM_DATA_VALUE, inputStream);
        }
    }

}
