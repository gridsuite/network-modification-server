/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.RemoveSubstation;
import com.powsybl.iidm.modification.topology.RemoveSubstationBuilder;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.dto.HvdcLccDeletionInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.entities.equipment.deletion.HvdcLccDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.ShuntCompensatorSelectionEmbeddable;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class EquipmentDeletionTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .stashed(false)
                .equipmentType(IdentifiableType.LOAD)
                .equipmentId("v1load")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .stashed(false)
                .equipmentType(IdentifiableType.GENERATOR)
                .equipmentId("idGenerator")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getLoad("v1load"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLoad("v1load"));
    }

    @Test
    void testOkWhenRemovingIsolatedEquipment() throws Exception {

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .stashed(false)
                .equipmentType(IdentifiableType.LOAD)
                .equipmentId("v5load")
                .build();
        String equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, null);

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk());

        var v5 = getNetwork().getVoltageLevel("v5");
        assertNull(v5.getNodeBreakerView().getTerminal(2));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // delete load (fail because the load is not found)
        EquipmentDeletionInfos equipmentDeletionInfos = (EquipmentDeletionInfos) buildModification();
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        String body = getJsonBody(equipmentDeletionInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(body).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=notFoundLoad not found or of bad type").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    private void deleteHvdcLineWithShuntCompensator(String shuntNameToBeRemoved, boolean selected, int side, boolean warningCase) throws Exception {
        final String hvdcLineName = "hvdcLine"; // this line uses LCC converter stations
        assertNotNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(warningCase, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);

        NetworkModificationResult.ApplicationStatus expectedStatus = warningCase ?
                NetworkModificationResult.ApplicationStatus.WITH_WARNINGS :
                NetworkModificationResult.ApplicationStatus.ALL_OK;

        List<ShuntCompensatorSelectionEmbeddable> shuntData = List.of(new ShuntCompensatorSelectionEmbeddable(shuntNameToBeRemoved, selected));
        HvdcLccDeletionEntity hvdcLccDeletionEntity = side == 1 ?
                new HvdcLccDeletionEntity(shuntData, null) :
                new HvdcLccDeletionEntity(null, shuntData);
        HvdcLccDeletionInfos hvdcLccDeletionInfos = hvdcLccDeletionEntity.toDto();
        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .stashed(false)
                .equipmentType(IdentifiableType.HVDC_LINE)
                .equipmentId(hvdcLineName)
                .equipmentInfos(hvdcLccDeletionInfos)
                .build();
        String equipmentDeletionInfosJson = getJsonBody(equipmentDeletionInfos, null);

        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted()).andReturn();
        mvcResult = mockMvc.perform(asyncDispatch(mvcResult))
                .andExpect(status().isOk())
                .andReturn();
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());
        assertEquals(expectedStatus, extractApplicationStatus(networkModificationsResult).getFirst());

        assertNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(selected, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);
    }

    @CsvSource({"true,  1", "true,  2", "false, 1", "false, 2"})
    @ParameterizedTest(name = ParameterizedTest.ARGUMENTS_WITH_NAMES_PLACEHOLDER)
    void testDeleteHvdcWithLCCWithShuntCompensator(final boolean selected, final int side) throws Exception {
        deleteHvdcLineWithShuntCompensator("v2shunt", selected, side, false);
    }

    @Test
    void testDeleteHvdcWithLCCWithAlreadyDeletedShuntCompensator() throws Exception {
        // we select a nonexistent shunt: will produce a warning
        deleteHvdcLineWithShuntCompensator("deletedOrMissingShuntId", true, 1, true);
    }

    @Test
    void testRemoveUnknownSubstation() {
        Network network = Network.create("empty", "test");
        RemoveSubstation removeSubstation = new RemoveSubstationBuilder().withSubstationId("unknownSubstation").build();
        PowsyblException e = assertThrows(PowsyblException.class, () -> removeSubstation.apply(network, true, ReportNode.NO_OP));
        assertEquals("Substation not found: unknownSubstation", e.getMessage());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("EQUIPMENT_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1load", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("EQUIPMENT_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator", createdValues.get("equipmentId"));
    }
}
