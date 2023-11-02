/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.SubstationCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class SubstationCreationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return SubstationCreationInfos.builder()
                .equipmentId("SubstationId")
                .equipmentName("SubstationName")
                .substationCountry(Country.AF)
                .properties(Map.of("DEMO", "DemoC"))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return SubstationCreationInfos.builder()
                .equipmentId("SubstationIdEdited")
                .equipmentName("SubstationNameEdited")
                .substationCountry(Country.CI)
                .properties(Map.of("DEMO", "DemoU"))
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
    public void testCreateWithErrors() throws Exception {
        SubstationCreationInfos substationCreationInfos = (SubstationCreationInfos) buildModification();
        substationCreationInfos.setEquipmentId("");
        String substationCreationInfosJson = mapper.writeValueAsString(substationCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(substationCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", substationCreationInfos.getErrorType().name(), reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "SUBSTATION_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"SubstationId\"}");
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "SUBSTATION_CREATION");
        assertEquals(modificationInfos.getMessageValues(), "{\"equipmentId\":\"SubstationIdEdited\"}");
    }
}
