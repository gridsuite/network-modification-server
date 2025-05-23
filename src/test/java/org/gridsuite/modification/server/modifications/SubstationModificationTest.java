/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Tag("IntegrationTest")
class SubstationModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return SubstationModificationInfos.builder()
            .stashed(false)
            .equipmentId("s3")
            .equipmentName(new AttributeModification<>("newName", OperationType.SET))
            .country(new AttributeModification<>(Country.BQ, OperationType.SET))
            .properties(List.of(FreePropertyInfos.builder().name("p1").value("v1").build(), // new
                FreePropertyInfos.builder().name("p2").value("v2").build(), // new
                FreePropertyInfos.builder().name("region").value("south").build(), // update
                FreePropertyInfos.builder().name("tso").value("").deletionMark(true).build()))// deletion
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return SubstationModificationInfos.builder()
            .equipmentId("s3Edited")
            .stashed(false)
            .equipmentName(new AttributeModification<>("newNameEdited1", OperationType.SET))
            .country(new AttributeModification<>(Country.JP, OperationType.SET))
            .properties(null)
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Substation modifiedStation = getNetwork().getSubstation("s3");
        assertNotNull(modifiedStation);
        assertEquals("newName", modifiedStation.getOptionalName().orElse(""));
        assertEquals(Country.BQ, modifiedStation.getCountry().orElse(Country.ZW));
        // 2 properties added and 1 updated, 'tso' removed
        assertEquals(Set.of("p1", "p2", "region"), modifiedStation.getPropertyNames());
        assertEquals("v1", modifiedStation.getProperty("p1", ""));
        assertEquals("v2", modifiedStation.getProperty("p2", ""));
        assertEquals("south", modifiedStation.getProperty("region", ""));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        Substation modifiedStation = getNetwork().getSubstation("s3");
        assertNotNull(modifiedStation);
        assertEquals("s3", modifiedStation.getOptionalName().orElse(""));
        assertEquals(Country.FR, modifiedStation.getCountry().orElse(Country.ZW));
        // property 'tso' is back, and region got its origin value
        assertEquals(Set.of("tso", "region"), modifiedStation.getPropertyNames());
        assertEquals("rtefrance", modifiedStation.getProperty("tso", ""));
        assertEquals("west", modifiedStation.getProperty("region", ""));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // Try to modify an unknown substation
        SubstationModificationInfos infos = SubstationModificationInfos.builder()
                .equipmentId("unknown")
                .country(new AttributeModification<>(Country.JP, OperationType.SET))
                .build();
        String infosJson = getJsonBody(infos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(infosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("SUBSTATION_NOT_FOUND : Substation unknown does not exist in network", ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SUBSTATION_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("s3", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SUBSTATION_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("s3Edited", updatedValues.get("equipmentId"));
    }
}
