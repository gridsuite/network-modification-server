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
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.service.LoadFlowService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Tag("IntegrationTest")
public class BalancesAdjustmentTest extends AbstractNetworkModificationTest {
    private static final UUID LOADFLOW_PARAMETERS_UUID = UUID.randomUUID();

    @MockBean
    private LoadFlowService loadFlowService;

    @BeforeEach
    public void setupLoadFlowServiceMock() {
        when(loadFlowService.getLoadFlowParametersInfos(LOADFLOW_PARAMETERS_UUID))
                .thenReturn(LoadFlowParametersInfos.builder()
                        .provider("OpenLoadFlow")
                        .commonParameters(LoadFlowParameters.load())
                        .specificParametersPerProvider(Map.of("OpenLoadFlow", Map.of(
                                "key1", "value1"
                        )))
                        .build());
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
