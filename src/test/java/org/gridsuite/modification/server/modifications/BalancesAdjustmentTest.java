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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.service.LoadFlowService;
import org.gridsuite.modification.server.NetworkModificationServerException;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.HttpStatusCodeException;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Tag("IntegrationTest")
class BalancesAdjustmentTest extends AbstractNetworkModificationTest {
    private static final UUID LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();
    private static final UUID NON_EXISTENT_LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();
    private static final UUID ERROR_LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();

    @MockBean
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
                .thenThrow(new NetworkModificationException(
                        NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR,
                        "Internal server error"));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("fourSubstationsNb_country 2_N1.xiidm", getClass().getResourceAsStream("/fourSubstationsNb_country 2_N1.xiidm"));
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

    @Test
    @Override // We have to override this method to remove check on errors
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
        ModificationInfos createdModification = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(createdModification).recursivelyEquals(modificationToCreate);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
        assertAfterNetworkModificationCreation();

        ModificationInfos createdModificationWithOnlyMetadata = modificationRepository.getModifications(TEST_GROUP_ID, true, true).get(0);
        testCreationModificationMessage(createdModificationWithOnlyMetadata);
    }

    /**
     * Test LoadFlowService.getLoadFlowParametersInfos() method for successful case
     */
    @Test
    void testGetLoadFlowParametersInfosSuccess() {
        LoadFlowParametersInfos result = loadFlowService.getLoadFlowParametersInfos(LOADFLOW_PARAMETERS_UUID);

        assertNotNull(result);
        assertEquals("OpenLoadFlow", result.getProvider());
        assertNotNull(result.getCommonParameters());
        assertNotNull(result.getSpecificParametersPerProvider());
        assertTrue(result.getSpecificParametersPerProvider().containsKey("OpenLoadFlow"));
    }

    /**
     * Test LoadFlowService.getLoadFlowParametersInfos() method for not found case (404)
     */
    @Test
    void testGetLoadFlowParametersInfosNotFound() {
        LoadFlowParametersInfos result = loadFlowService.getLoadFlowParametersInfos(NON_EXISTENT_LOADFLOW_PARAMETERS_UUID);

        assertNull(result);
    }

    /**
     * Test LoadFlowService.getLoadFlowParametersInfos() method for server error case
     */
    @Test
    void testGetLoadFlowParametersInfosServerError() {
        NetworkModificationException exception = assertThrows(
                NetworkModificationException.class,
                () -> loadFlowService.getLoadFlowParametersInfos(ERROR_LOADFLOW_PARAMETERS_UUID)
        );

        assertEquals(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR, exception.getType());
        assertEquals("LOAD_FLOW_PARAMETERS_FETCH_ERROR : Internal server error", exception.getMessage());
    }

    /**
     * Test NetworkModificationServerException.handleChangeError() method with empty response body
     */
    @Test
    void testHandleChangeErrorWithEmptyResponseBody() {
        HttpStatusCodeException httpException = new HttpStatusCodeException(HttpStatus.INTERNAL_SERVER_ERROR) {
            @NotNull
            @Override
            public String getResponseBodyAsString() {
                return "";
            }
        };

        NetworkModificationException result = NetworkModificationServerException.handleChangeError(
                httpException,
                NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR
        );

        assertEquals(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR, result.getType());
        assertEquals("LOAD_FLOW_PARAMETERS_FETCH_ERROR : 500 INTERNAL_SERVER_ERROR", result.getMessage());
    }

    /**
     * Test NetworkModificationServerException.handleChangeError() method with JSON response body containing message
     */
    @Test
    void testHandleChangeErrorWithJsonResponseBody() {
        HttpStatusCodeException httpException = new HttpStatusCodeException(HttpStatus.BAD_REQUEST) {
            @NotNull
            @Override
            public String getResponseBodyAsString() {
                return "{\"message\": \"Invalid parameters provided\", \"code\": 400}";
            }
        };

        NetworkModificationException result = NetworkModificationServerException.handleChangeError(
                httpException,
                NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR
        );

        assertEquals(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR, result.getType());
        assertEquals("LOAD_FLOW_PARAMETERS_FETCH_ERROR : Invalid parameters provided", result.getMessage());
    }

    /**
     * Test NetworkModificationServerException.handleChangeError() method with plain text response body
     */
    @Test
    void testHandleChangeErrorWithPlainTextResponseBody() {
        HttpStatusCodeException httpException = new HttpStatusCodeException(HttpStatus.NOT_FOUND) {
            @NotNull
            @Override
            public String getResponseBodyAsString() {
                return "Resource not found";
            }
        };

        NetworkModificationException result = NetworkModificationServerException.handleChangeError(
                httpException,
                NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR
        );

        assertEquals(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR, result.getType());
        assertEquals("LOAD_FLOW_PARAMETERS_FETCH_ERROR : Resource not found", result.getMessage());
    }

    /**
     * Test NetworkModificationServerException.handleChangeError() method with invalid JSON response body
     */
    @Test
    void testHandleChangeErrorWithInvalidJsonResponseBody() {
        HttpStatusCodeException httpException = new HttpStatusCodeException(HttpStatus.INTERNAL_SERVER_ERROR) {
            @NotNull
            @Override
            public String getResponseBodyAsString() {
                return "{invalid json structure";
            }
        };

        NetworkModificationException result = NetworkModificationServerException.handleChangeError(
                httpException,
                NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR
        );

        assertEquals(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR, result.getType());
        assertEquals("LOAD_FLOW_PARAMETERS_FETCH_ERROR : {invalid json structure", result.getMessage());
    }

    /**
     * Test NetworkModificationServerException.handleChangeError() method with JSON response body without message field
     */
    @Test
    void testHandleChangeErrorWithJsonResponseBodyWithoutMessage() {
        HttpStatusCodeException httpException = new HttpStatusCodeException(HttpStatus.CONFLICT) {
            @NotNull
            @Override
            public String getResponseBodyAsString() {
                return "{\"error\": \"Conflict occurred\", \"timestamp\": \"2025-01-01T10:00:00Z\"}";
            }
        };

        NetworkModificationException result = NetworkModificationServerException.handleChangeError(
                httpException,
                NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR
        );

        assertEquals(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR, result.getType());
        assertEquals("LOAD_FLOW_PARAMETERS_FETCH_ERROR : {\"error\": \"Conflict occurred\", \"timestamp\": \"2025-01-01T10:00:00Z\"}", result.getMessage());
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
