/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.RemoveSubstation;
import com.powsybl.iidm.modification.topology.RemoveSubstationBuilder;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.HvdcLccDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.entities.equipment.deletion.ShuntCompensatorSelectionEmbeddable;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class EquipmentDeletionTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .equipmentType("LOAD")
                .equipmentId("v1load")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .equipmentType("GENERATOR")
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
    public void testOkWhenRemovingIsolatedEquipment() throws Exception {

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType("LOAD")
                .equipmentId("v5load")
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        var v5 = getNetwork().getVoltageLevel("v5");
        assertNull(v5.getNodeBreakerView().getTerminal(2));
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        // delete load (fail because the load is not found)
        EquipmentDeletionInfos equipmentDeletionInfos = (EquipmentDeletionInfos) buildModification();
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=notFoundLoad not found or of bad type").getMessage(),
                equipmentDeletionInfos.getErrorType().name(), reportService);
    }

    @SneakyThrows
    private void deleteHvdcLineWithShuntCompensator(String shuntNameToBeRemoved, boolean selected, int side, boolean warningCase) {
        final String hvdcLineName = "hvdcLine"; // this line uses LCC converter stations
        assertNotNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(warningCase, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);

        NetworkModificationResult.ApplicationStatus expectedStatus = warningCase ?
                NetworkModificationResult.ApplicationStatus.WITH_WARNINGS :
                NetworkModificationResult.ApplicationStatus.ALL_OK;

        List<ShuntCompensatorSelectionEmbeddable> shuntData = List.of(new ShuntCompensatorSelectionEmbeddable(shuntNameToBeRemoved, selected));
        HvdcLccDeletionInfos hvdcLccDeletionInfos = side == 1 ?
                new HvdcLccDeletionInfos(shuntData, null) :
                new HvdcLccDeletionInfos(null, shuntData);
        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType("HVDC_LINE")
                .equipmentId(hvdcLineName)
                .equipmentInfos(hvdcLccDeletionInfos)
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        Optional<NetworkModificationResult> modifResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(modifResult.isPresent());
        assertEquals(expectedStatus, modifResult.get().getApplicationStatus());

        assertNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(selected, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);
    }

    @Test
    public void testDeleteHvdcWithLCCWithShuntCompensatorSelectedSide1() {
        deleteHvdcLineWithShuntCompensator("v2shunt", true, 1, false);
    }

    @Test
    public void testDeleteHvdcWithLCCWithShuntCompensatorSelectedSide2() {
        deleteHvdcLineWithShuntCompensator("v2shunt", true, 2, false);
    }

    @Test
    public void testDeleteHvdcWithLCCWithShuntCompensatorNotSelectedSide1() {
        deleteHvdcLineWithShuntCompensator("v2shunt", false, 1, false);
    }

    @Test
    public void testDeleteHvdcWithLCCWithShuntCompensatorNotSelectedSide2() {
        deleteHvdcLineWithShuntCompensator("v2shunt", false, 2, false);
    }

    @Test
    public void testDeleteHvdcWithLCCWithAlreadyDeletedShuntCompensator() {
        // we select an unexisting shunt: will produce a warning
        deleteHvdcLineWithShuntCompensator("deletedOrMissingShuntId", true, 1, true);
    }

    @Test
    public void testRemoveUnknownSubstation() {
        Network network = Network.create("empty", "test");
        RemoveSubstation removeSubstation = new RemoveSubstationBuilder().withSubstationId("unknownSubstation").build();
        PowsyblException e = assertThrows(PowsyblException.class, () -> removeSubstation.apply(network, true, Reporter.NO_OP));
        assertEquals("Substation not found: unknownSubstation", e.getMessage());
    }
}
