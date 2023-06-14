/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.SubstationFreePropertyInfos;
import org.gridsuite.modification.server.dto.SubstationModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class SubstationModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return SubstationModificationInfos.builder()
            .equipmentId("s3")
            .equipmentName(new AttributeModification<>("newName", OperationType.SET))
            .substationCountry(new AttributeModification<>(Country.BQ, OperationType.SET))
            .properties(List.of(SubstationFreePropertyInfos.builder().name("p1").value("v1").build(), // new
                    SubstationFreePropertyInfos.builder().name("p2").value("v2").build(), // new
                    SubstationFreePropertyInfos.builder().name("region").value("south").build(), // update
                    SubstationFreePropertyInfos.builder().name("tso").value("").deletionMark(true).build()))// deletion
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return SubstationModificationInfos.builder()
            .equipmentId("s3")
            .equipmentName(new AttributeModification<>("newNameEdited1", OperationType.SET))
            .substationCountry(new AttributeModification<>(Country.JP, OperationType.SET))
            .properties(null)
            .build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
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
    protected void assertNetworkAfterDeletion() {
        Substation modifiedStation = getNetwork().getSubstation("s3");
        assertNotNull(modifiedStation);
        assertEquals("s3", modifiedStation.getOptionalName().orElse(""));
        assertEquals(Country.FR, modifiedStation.getCountry().orElse(Country.ZW));
        // property 'tso' is back, and region got its origin value
        assertEquals(Set.of("tso", "region"), modifiedStation.getPropertyNames());
        assertEquals("rtefrance", modifiedStation.getProperty("tso", ""));
        assertEquals("west", modifiedStation.getProperty("region", ""));
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // Try to modify an unknown substation
        SubstationModificationInfos infos = SubstationModificationInfos.builder()
                .equipmentId("unknown")
                .substationCountry(new AttributeModification<>(Country.JP, OperationType.SET))
                .build();
        String infosJson = mapper.writeValueAsString(infos);
        mockMvc.perform(post(getNetworkModificationUri()).content(infosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("SUBSTATION_NOT_FOUND : Substation unknown does not exist in network", infos.getErrorType().name(), reportService);
    }
}
