/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherSubstationModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

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
            .properties(List.of(SubstationFreePropertyInfos.builder().name("p1").value("v1").build(),
                    SubstationFreePropertyInfos.builder().name("p2").value("v2").build(),
                    SubstationFreePropertyInfos.builder().name("tso").value("").deletionMark(true).build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return SubstationModificationInfos.builder()
            .equipmentId("s3")
            .equipmentName(new AttributeModification<>("newNameEdited1", OperationType.SET))
            .substationCountry(new AttributeModification<>(Country.JP, OperationType.SET))
            .properties(List.of())
            .build();
    }

    @Override
    protected MatcherSubstationModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherSubstationModificationInfos.createMatcherLoadModificationInfos((SubstationModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        Substation modifiedStation = getNetwork().getSubstation("s3");
        assertNotNull(modifiedStation);
        assertEquals("newName", modifiedStation.getOptionalName().orElse(""));
        assertEquals(Country.BQ, modifiedStation.getCountry().orElse(Country.ZW));
        // 2 properties added, 'tso' removed
        assertEquals(Set.of("p1", "p2"), modifiedStation.getPropertyNames());
        assertEquals("v1", modifiedStation.getProperty("p1", ""));
        assertEquals("v2", modifiedStation.getProperty("p2", ""));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        Substation modifiedStation = getNetwork().getSubstation("s3");
        assertNotNull(modifiedStation);
        assertEquals("s3", modifiedStation.getOptionalName().orElse(""));
        assertEquals(Country.FR, modifiedStation.getCountry().orElse(Country.ZW));
        // property 'tso' is back
        assertEquals(Set.of("tso"), modifiedStation.getPropertyNames());
        assertEquals("rtefrance", modifiedStation.getProperty("tso", ""));
    }
/*
    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // Unset an attribute that should not be null
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        String loadModificationInfosJson = mapper.writeValueAsString(loadModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadModificationInfosJson)
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().is5xxServerError(),
                        content().string(
                                new NetworkModificationException(MODIFY_LOAD_ERROR, "Load 'v1load': load type is null")
                                        .getMessage()));
    }*/
}
