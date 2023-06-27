/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ShuntCompensatorSelectionInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
    protected MatcherEquipmentDeletionInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos((EquipmentDeletionInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getLoad("v1load"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLoad("v1load"));
    }

    @SneakyThrows
    @Test
    public void testOkWhenRemovingIsolatedEquipment() {

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

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // delete load (fail because the load is not found)
        EquipmentDeletionInfos equipmentDeletionInfos = (EquipmentDeletionInfos) buildModification();
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=notFoundLoad not found or of bad type").getMessage(),
                equipmentDeletionInfos.getErrorType().name(), reportService);

        // try to delete substation (Internal error because the substation is still connected)
        equipmentDeletionInfos.setEquipmentType("SUBSTATION");
        equipmentDeletionInfos.setEquipmentId("s2");
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("The substation s2 is still connected to another substation", equipmentDeletionInfos.getErrorType().name(), reportService);
        assertNotNull(getNetwork().getSubstation("s2"));
    }

    @SneakyThrows
    private void deleteHvdcLineWithShuntCompensator(boolean selected) {
        final String hvdcLineName = "hvdcLine"; // this line uses LCC converter stations
        final String shuntNameToBeRemoved = "v2shunt"; // to be removed if selected
        assertNotNull(getNetwork().getHvdcLine(hvdcLineName));
        assertNotNull(getNetwork().getShuntCompensator(shuntNameToBeRemoved));

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .equipmentType("HVDC_LINE")
                .equipmentId(hvdcLineName)
                .mcsOnSide2(List.of(new ShuntCompensatorSelectionInfos(shuntNameToBeRemoved, selected)))
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        Optional<NetworkModificationResult> modifResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(modifResult.isPresent());
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, modifResult.get().getApplicationStatus());

        assertNull(getNetwork().getHvdcLine(hvdcLineName));
        assertEquals(selected, getNetwork().getShuntCompensator(shuntNameToBeRemoved) == null);
    }

    @Test
    public void testDeleteHvdcWithLCCWithShuntCompensatorSelected() {
        deleteHvdcLineWithShuntCompensator(true);
    }

    @Test
    public void testDeleteHvdcWithLCCWithShuntCompensatorNotSelected() {
        deleteHvdcLineWithShuntCompensator(false);
    }
}
