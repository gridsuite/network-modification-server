/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.loadflow.LoadFlowParameters;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.service.LoadFlowService;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.HttpServerErrorException;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.TestUtils.runRequestAsync;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Tag("IntegrationTest")
@DisableElasticsearch
class BalancesAdjustmentTest extends AbstractNetworkModificationTest {
    private static final UUID LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();
    private static final UUID NON_EXISTENT_LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();
    private static final UUID ERROR_LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();

    @MockitoBean
    private LoadFlowService loadFlowService;

    @BeforeEach
    void setupLoadFlowServiceMock() {
        when(loadFlowService.getLoadFlowParametersInfos(LOADFLOW_PARAMETERS_UUID))
                .thenReturn(LoadFlowParametersInfos.builder()
                        .provider("OpenLoadFlow")
                        .commonParameters(LoadFlowParameters.load())
                        .specificParametersPerProvider(Map.of("OpenLoadFlow", Map.of(
                                "key1", "value1"
                        )))
                        .build());

        // Mock for non-existent parameters (404 case)
        when(loadFlowService.getLoadFlowParametersInfos(NON_EXISTENT_LOADFLOW_PARAMETERS_UUID))
                .thenReturn(null);

        // Mock for server error case
        when(loadFlowService.getLoadFlowParametersInfos(ERROR_LOADFLOW_PARAMETERS_UUID))
                .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR, "Internal server error"));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = Network.read("fourSubstationsNb_country 2_N1.xiidm", getClass().getResourceAsStream("/fourSubstationsNb_country 2_N1.xiidm"));
        String initialVariant = network.getVariantManager().getWorkingVariantId();
        String modificationVariant = "ModificationVariant";
        network.getVariantManager().cloneVariant(initialVariant, modificationVariant);
        network.getVariantManager().setWorkingVariant(modificationVariant);
        return network;
    }

    @Override
    protected ModificationInfos buildModification() {
        return BalancesAdjustmentModificationInfos.builder()
                .areas(List.of(
                        BalancesAdjustmentAreaInfos.builder()
                                .name("FR")
                                .countries(List.of(Country.FR))
                                .netPosition(-45d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build(),
                        BalancesAdjustmentAreaInfos.builder()
                                .name("NE")
                                .countries(List.of(Country.NE))
                                .netPosition(-54d)
                                .shiftType(ShiftType.BALANCED)
                                .shiftEquipmentType(ShiftEquipmentType.GENERATOR)
                                .build(),
                        BalancesAdjustmentAreaInfos.builder()
                                .name("GE")
                                .countries(List.of(Country.GE))
                                .netPosition(0d)
                                .shiftType(ShiftType.PROPORTIONAL)
                                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                                .build(),
                        BalancesAdjustmentAreaInfos.builder()
                                .name("AU")
                                .countries(List.of(Country.AU))
                                .netPosition(100d)
                                .shiftType(ShiftType.BALANCED)
                                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                                .build()
                ))
                .withLoadFlow(true)
                .loadFlowParametersId(LOADFLOW_PARAMETERS_UUID)
                .build();
    }

    private ApplicationStatus createModification(ModificationInfos modification) throws Exception {
        MvcResult mvcResult = runRequestAsync(mockMvc, post(getNetworkModificationUri())
                .content(getJsonBody(modification, null))
                .contentType(MediaType.APPLICATION_JSON), status().isOk());
        NetworkModificationsResult result = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        return extractApplicationStatus(result).getFirst();
    }

    @Test
    void testCreateLoadsLoadFlowParametersOnce() throws Exception {
        assertEquals(ApplicationStatus.ALL_OK, createModification(buildModification()));

        assertAfterNetworkModificationCreation();
        verify(loadFlowService).getLoadFlowParametersInfos(LOADFLOW_PARAMETERS_UUID);
    }

    @Test
    void testCreateWithLoadFlowParametersNotFound() throws Exception {
        BalancesAdjustmentModificationInfos modification = (BalancesAdjustmentModificationInfos) buildModification();
        modification.setLoadFlowParametersId(NON_EXISTENT_LOADFLOW_PARAMETERS_UUID);

        assertEquals(ApplicationStatus.ALL_OK, createModification(modification));

        assertLogMessage("Using default load flow parameters: Load flow parameters with id " + NON_EXISTENT_LOADFLOW_PARAMETERS_UUID + " not found",
                "network.modification.balancesAdjustment.usingDefaultLoadFlowParameters", reportService);
    }

    @Test
    void testCreateWithLoadFlowServerError() throws Exception {
        BalancesAdjustmentModificationInfos modification = (BalancesAdjustmentModificationInfos) buildModification();
        modification.setLoadFlowParametersId(ERROR_LOADFLOW_PARAMETERS_UUID);

        // the parameters cannot be loaded: the modification fails without changing the network
        assertEquals(ApplicationStatus.WITH_ERRORS, createModification(modification));

        assertAfterNetworkModificationDeletion();
    }

    @Test
    void testCreateWithoutLoadFlowDoesNotLoadParameters() throws Exception {
        BalancesAdjustmentModificationInfos modification = (BalancesAdjustmentModificationInfos) buildModification();
        modification.setWithLoadFlow(false);

        assertEquals(ApplicationStatus.ALL_OK, createModification(modification));

        verify(loadFlowService, never()).getLoadFlowParametersInfos(any());
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BalancesAdjustmentModificationInfos.builder()
                .areas(List.of(
                        BalancesAdjustmentAreaInfos.builder()
                                .name("FR")
                                .countries(List.of(Country.FR))
                                .netPosition(-45d)
                                .shiftType(ShiftType.BALANCED)
                                .shiftEquipmentType(ShiftEquipmentType.LOAD)
                                .build()
                ))
                .countriesToBalance(List.of(Country.FR))
                .maxNumberIterations(1)
                .thresholdNetPosition(30d)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(-58.4d, getNetwork().getGenerator("GH1").getTerminal().getP(), 0.1);
        assertEquals(-36d, getNetwork().getGenerator("GH2").getTerminal().getP(), 0.1);
        assertEquals(-101.8d, getNetwork().getGenerator("GH3").getTerminal().getP(), 0.1);
        assertEquals(-100d, getNetwork().getGenerator("GTH1").getTerminal().getP(), 0.1);
        assertEquals(-146.9d, getNetwork().getGenerator("GTH2").getTerminal().getP(), 0.1);

        assertEquals(80.2d, getNetwork().getLoad("LD1").getTerminal().getP(), 0.1);
        assertEquals(60.2d, getNetwork().getLoad("LD2").getTerminal().getP(), 0.1);
        assertEquals(60.2d, getNetwork().getLoad("LD3").getTerminal().getP(), 0.1);
        assertEquals(40.1d, getNetwork().getLoad("LD4").getTerminal().getP(), 0.1);
        assertEquals(200.5d, getNetwork().getLoad("LD5").getTerminal().getP(), 0.1);
        assertEquals(0d, getNetwork().getLoad("LD6").getTerminal().getP(), 0.1);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(-85.4d, getNetwork().getGenerator("GH1").getTerminal().getP(), 0.1);
        assertEquals(-90d, getNetwork().getGenerator("GH2").getTerminal().getP(), 0.1);
        assertEquals(-155.7d, getNetwork().getGenerator("GH3").getTerminal().getP(), 0.1);
        assertEquals(-100d, getNetwork().getGenerator("GTH1").getTerminal().getP(), 0.1);
        assertEquals(-251d, getNetwork().getGenerator("GTH2").getTerminal().getP(), 0.1);

        assertEquals(80.0d, getNetwork().getLoad("LD1").getTerminal().getP(), 0.1);
        assertEquals(60.0d, getNetwork().getLoad("LD2").getTerminal().getP(), 0.1);
        assertEquals(60.0d, getNetwork().getLoad("LD3").getTerminal().getP(), 0.1);
        assertEquals(40.0d, getNetwork().getLoad("LD4").getTerminal().getP(), 0.1);
        assertEquals(200.0d, getNetwork().getLoad("LD5").getTerminal().getP(), 0.1);
        assertEquals(240d, getNetwork().getLoad("LD6").getTerminal().getP(), 0.1);
    }
}
