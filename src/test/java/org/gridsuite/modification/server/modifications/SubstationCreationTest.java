/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class SubstationCreationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return SubstationCreationInfos.builder()
                .stashed(false)
                .equipmentId("SubstationId")
                .equipmentName("SubstationName")
                .country(Country.AF)
                .properties(List.of(FreePropertyInfos.builder().name("DEMO").value("DemoC").build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return SubstationCreationInfos.builder()
                .stashed(false)
                .equipmentId("SubstationIdEdited")
                .equipmentName("SubstationNameEdited")
                .country(Country.CI)
                .properties(List.of(FreePropertyInfos.builder().name("DEMO").value("DemoU").build()))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Substation substation = getNetwork().getSubstation("SubstationId");
        assertNotNull(substation);
        assertEquals("DemoC", substation.getProperty("DEMO"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getSubstation("SubstationId"));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) buildModification();
        substationCreationInfos.setEquipmentId("");
        String substationCreationInfosJson = mapper.writeValueAsString(substationCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(substationCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", substationCreationInfos.getErrorType().name(), reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SUBSTATION_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("SubstationId", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SUBSTATION_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("SubstationIdEdited", updatedValues.get("equipmentId"));
    }
}
