/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.server.dto.*;
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
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest
public class LinesAttachToSplitLinesTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final String CREATE_URI_STRING = "/v1/networks/{networkUuid}/lines-attach-to-split-lines?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String UPDATE_URI_STRING = "/v1/modifications/%s/lines-attach-to-split-lines-creation";
    private static final String DELETE_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "/modifications";
    private static final String COPY_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY";

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

    private ObjectWriter objectWriter;
    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });
        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
                .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
    }

    @After
    public void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
    }

    private List<LinesAttachToSplitLinesInfos> getModifications() {
        return modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(LinesAttachToSplitLinesInfos.class::cast).collect(Collectors.toList());
    }

    private void createModification() throws Exception {
        LinesAttachToSplitLinesInfos linesAttachToSplitLines = LinesAttachToSplitLinesInfos.builder()
                .type(ModificationType.LINES_ATTACH_TO_SPLIT_LINES)
                .lineToAttachTo1Id("line1")
                .lineToAttachTo2Id("line2")
                .attachedLineId("line3")
                .voltageLevelId("v4")
                .bbsBusId("1.A")
                .replacingLine1Id("nl4")
                .replacingLine1Name("NewLine4")
                .replacingLine2Id("nl5")
                .replacingLine2Name("NewLine5")
                .build();
        String linesAttachToSplitLinesJson = objectWriter.writeValueAsString(linesAttachToSplitLines);
        mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(linesAttachToSplitLinesJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    public void testCreate() throws Exception {
        createModification();

        List<LinesAttachToSplitLinesInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LinesAttachToSplitLinesInfos modification = modifications.get(0);
        assertEquals("line1", modification.getLineToAttachTo1Id());
        assertEquals("line2", modification.getLineToAttachTo2Id());
        assertEquals("line3", modification.getAttachedLineId());
        assertEquals("v4", modification.getVoltageLevelId());
        assertEquals("NewLine4", modification.getReplacingLine1Name());
        assertEquals("NewLine5", modification.getReplacingLine2Name());
    }

    @Test
    public void testUpdate() throws Exception {
        // 1- First create a modification
        createModification();
        List<LinesAttachToSplitLinesInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LinesAttachToSplitLinesInfos firstModification = modifications.get(0);

        // 2- Then update it
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesUpdate = LinesAttachToSplitLinesInfos.builder()
                .type(ModificationType.LINES_ATTACH_TO_SPLIT_LINES)
                .lineToAttachTo1Id("line1")
                .lineToAttachTo2Id("line2")
                .attachedLineId("line3")
                .voltageLevelId("v4")
                .bbsBusId("1.A")
                .replacingLine1Id("nl4")
                .replacingLine1Name("new line4")
                .replacingLine2Id("nl5")
                .replacingLine2Name("new line5")
                .build();
        String linesAttachToSplitLinesUpdateJson = objectWriter.writeValueAsString(linesAttachToSplitLinesUpdate);
        mockMvc.perform(put(String.format(UPDATE_URI_STRING, firstModification.getUuid())).content(linesAttachToSplitLinesUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        modifications = getModifications();
        assertEquals(1, modifications.size());
        firstModification = modifications.get(0);
        // modification has changed
        assertEquals("new line4", firstModification.getReplacingLine1Name());
        assertEquals("new line5", firstModification.getReplacingLine2Name());
    }

    @Test
    public void testDeletion() throws Exception {
        // 1- First create a modification
        createModification();

        List<LinesAttachToSplitLinesInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        LinesAttachToSplitLinesInfos firstModification = modifications.get(0);

        // 2- then remove it
        mockMvc.perform(delete(DELETE_URI_STRING).queryParam("modificationsUuids", firstModification.getUuid().toString())).andExpect(status().isOk());
        assertEquals(0, getModifications().size());
    }

    @Test
    public void testCopy() throws Exception {
        // 1- First create 1 modification
        createModification();

        List<LinesAttachToSplitLinesInfos> modifications = getModifications();
        assertEquals(1, getModifications().size());

        // 2- Then copy it in current group
        List<UUID> copyModificationUuidList = modifications.stream().map(LinesAttachToSplitLinesInfos::getUuid).collect(Collectors.toList());
        mockMvc.perform(put(COPY_URI_STRING)
                        .content(objectWriter.writeValueAsString(copyModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        modifications = getModifications();
        assertEquals(2, modifications.size());
        assertEquals(2, modifications.stream().filter(r -> r.getVoltageLevelId().equalsIgnoreCase("v4")).count());
        assertEquals(2, modifications.stream().filter(r -> r.getLineToAttachTo1Id().equalsIgnoreCase("line1")).count());
    }

    @Test
    public void testCreateErrorLineNotFound() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LinesAttachToSplitLinesInfos linesAttachToAbsentLine1 = LinesAttachToSplitLinesInfos.builder()
                .type(ModificationType.LINES_ATTACH_TO_SPLIT_LINES)
                .lineToAttachTo1Id("absent_line_id")
                .lineToAttachTo2Id("line2")
                .attachedLineId("line3")
                .voltageLevelId("v4")
                .bbsBusId("1.A")
                .replacingLine1Id("nl4")
                .replacingLine1Name("NewLine4")
                .replacingLine2Id("nl5")
                .replacingLine2Name("NewLine5")
                .build();

        String linesAttachToAbsentLine1Json = objectWriter.writeValueAsString(linesAttachToAbsentLine1);
        mvcResult = mockMvc.perform(post(CREATE_URI_STRING, TEST_NETWORK_ID).content(linesAttachToAbsentLine1Json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(new NetworkModificationException(LINE_ATTACH_ERROR, "Line absent_line_id is not found").getMessage(), resultAsString);
    }

    @Test
    public void testUpdateErrorModificationNotFound() throws Exception {
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesUpdate = LinesAttachToSplitLinesInfos.builder()
                .type(ModificationType.LINES_ATTACH_TO_SPLIT_LINES)
                .lineToAttachTo1Id("line1")
                .lineToAttachTo2Id("line2")
                .attachedLineId("line3")
                .voltageLevelId("v4")
                .bbsBusId("1.A")
                .replacingLine1Id("nl4")
                .replacingLine1Name("new line4")
                .replacingLine2Id("nl5")
                .replacingLine2Name("new line5")
                .build();
        String linesAttachToSplitLinesUpdateJson = objectWriter.writeValueAsString(linesAttachToSplitLinesUpdate);
        UUID uuidNotFound = UUID.randomUUID();
        MvcResult mvcResult = mockMvc.perform(put(String.format(UPDATE_URI_STRING, uuidNotFound)).content(linesAttachToSplitLinesUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", uuidNotFound)).getMessage());
    }
}
