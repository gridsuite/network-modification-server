/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.modification.NetworkModification;
import com.powsybl.iidm.modification.topology.ConnectVoltageLevelOnLineBuilder;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteVoltageLevelOnLineEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
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
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherDeleteVoltageLevelOnLineInfos.createMatcherDeleteVoltageLevelOnLineInfos;
import static org.gridsuite.modification.server.utils.NetworkUtil.createBusBarSection;
import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLine;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSubstation;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSwitch;
import static org.gridsuite.modification.server.utils.NetworkUtil.createVoltageLevel;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest()
public class DeleteVoltageLevelOnLineTest {
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();
    private static final String COPY_URI_STRING = "/v1/groups/" + TEST_GROUP_ID + "?action=COPY";
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + TEST_NETWORK_ID + URI_NETWORK_MODIF_PARAMS;
    protected static final String URI_NETWORK_MODIF_GET_PUT = URI_NETWORK_MODIF_BASE + "/";

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
    private NetworkModificationRepository networkModificationRepository;

    private ObjectWriter objectWriter;
    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = createNetwork();
            return network;
        });

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
                .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
        SQLStatementCountValidator.reset();
    }

    @After
    public void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
    }

    @Test
    public void createWithInvalidLineIdTest() throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        // test create with absent_line_id
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("ll")
                .replacingLine1Id("replacementLineId")
                .build();
        String json = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals("DELETE_VOLTAGE_LEVEL_ON_LINE_ERROR : Line ll is not found", resultAsString);
    }

    @Test
    public void testUpdate() throws Exception {
        // 1- First create a modification
        createTest();

        List<DeleteVoltageLevelOnLineInfos> modificationsAfterCreate = getModifications();
        assertEquals(1, modificationsAfterCreate.size());
        UUID modificationToUpdate = modificationsAfterCreate.get(0).getUuid();

        // 2- Then update it
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("line00")
                .lineToAttachTo2Id("line11")
                .replacingLine1Id("replacingLine2")
                .replacingLine1Name("replacingLine2")
                .build();
        String json = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationToUpdate).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        DeleteVoltageLevelOnLineInfos firstModification = modifications.get(0);
        // modification has changed
        assertEquals("line00", firstModification.getLineToAttachTo1Id());
        assertEquals("line11", firstModification.getLineToAttachTo2Id());
        assertEquals("replacingLine2", firstModification.getReplacingLine1Id());
        assertEquals("replacingLine2", firstModification.getReplacingLine1Name());
    }

    @Test
    public void testDelete() throws Exception {
        // 1- First create a modification
        createTest();

        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        // 2- then remove it
        mockMvc.perform(delete(URI_NETWORK_MODIF)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", modificationUuid.toString()))
                .andExpect(status().isOk()).andReturn();
        assertEquals(0, getModifications().size());
    }

    @Test
    public void testCopy() throws Exception {
        // 1- First create 1 modification
        createTest();

        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());

        // 2- Then copy them in current group
        List<UUID> copyModificationUuidList = modifications.stream().map(DeleteVoltageLevelOnLineInfos::getUuid).collect(Collectors.toList());
        mockMvc.perform(put(COPY_URI_STRING)
                        .content(objectWriter.writeValueAsString(copyModificationUuidList))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        modifications = getModifications();
        assertEquals(2, modifications.size());
    }

    @Test
    public void testModificationRepository() {
        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity1 = DeleteVoltageLevelOnLineEntity.toEntity(
                "lineId0", "lineId1", "line1Id", "line1Name");
        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity2 = DeleteVoltageLevelOnLineEntity.toEntity(
                "lineId4", "lineId5", "line3Id", "line3Name");
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(deleteVoltageLevelOnLineToEntity1, deleteVoltageLevelOnLineToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(0).getUuid()),
                createMatcherDeleteVoltageLevelOnLineInfos(
                        deleteVoltageLevelOnLineToEntity1.toModificationInfos()));

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(1).getUuid()),
                createMatcherDeleteVoltageLevelOnLineInfos(
                        deleteVoltageLevelOnLineToEntity2.toModificationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(deleteVoltageLevelOnLineToEntity1.getId(),
                deleteVoltageLevelOnLineToEntity2.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void readTest() throws Exception {
        // 1- First create 1 modification
        createTest();
        DeleteVoltageLevelOnLineInfos toCompareWith = DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .replacingLine1Id("replacementLineId")
                .build();
        List<DeleteVoltageLevelOnLineInfos> modifications = getModifications();
        assertEquals(1, modifications.size());
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();
        // 2- Perform get request to get modification
        mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + modificationUuid))
                .andExpect(status().isOk()).andReturn();
    }

    @Test
    public void createTest() throws Exception {
        var network = networkStoreService.getNetwork(TEST_NETWORK_ID);
        NetworkModification modification = new ConnectVoltageLevelOnLineBuilder()
                .withBusbarSectionOrBusId("l3d2")
                .withLine(network.getLine("l1"))
                .build();

        modification.apply(network);

        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .replacingLine1Id("replacementLineId")
                .replacingLine1Name("replacementLine")
                .build();
        String json = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        // new equipments in the network:
        assertNull(network.getLine("l3"));
    }

    private List<DeleteVoltageLevelOnLineInfos> getModifications() {
        return modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(DeleteVoltageLevelOnLineInfos.class::cast).collect(Collectors.toList());
    }

    private DeleteVoltageLevelOnLineInfos getDeleteVoltageLevelOnLineModification(UUID modificationUuid) {
        return (DeleteVoltageLevelOnLineInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private Network createNetwork() {
        Network network = new NetworkFactoryImpl().createNetwork(DeleteVoltageLevelOnLineTest.TEST_NETWORK_ID.toString(), "NetworkWithTeePoint");

        // VL1
        Substation s1 = createSubstation(network, "s1", null, Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v1, "bbs1", null, 0);
        createLoad(v1, "ld1", null, 2, 0., 0., "ld1", 0, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v1, "d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "br1", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL2
        Substation s2 = createSubstation(network, "s2", null, Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v2, "bbs2", null, 0);

        createGenerator(v2, "g2", 2, 42.1, 1.0, "g2", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v2, "br2", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL3
        Substation s3 = createSubstation(network, "s3", null, Country.FR);
        VoltageLevel v3 = createVoltageLevel(s3, "v3", null, TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v3, "bbs3", null, 0);

        createLoad(v3, "ld3", null, 2, 0., 0., "ld3", 3, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v3, "d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "br3", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // create lines
        createLine(network, "l1", null, "v1", "v2", 4, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "l1", 1, ConnectablePosition.Direction.TOP, "l1", 1, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l1d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l1br1", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v2, "l1d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l1br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l2", null, "v1", "v3", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "l2", 2, ConnectablePosition.Direction.TOP, "l2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l2d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l2br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v3, "l2d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "l2br3", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        return network;
    }
}
