/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.utils.MatcherLoadModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_LOAD_ERROR;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LoadModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadModificationInfos.builder()
            .id("v1load")
            .name(new AttributeModification<>("nameLoad1", OperationType.SET))
            .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
            .p0(new AttributeModification<>(200.0, OperationType.SET))
            .q0(new AttributeModification<>(30.0, OperationType.SET))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadModificationInfos.builder()
            .id("v1load")
            .name(new AttributeModification<>("nameLoadEdited1", OperationType.SET))
            .loadType(new AttributeModification<>(LoadType.AUXILIARY, OperationType.SET))
            .p0(new AttributeModification<>(300.0, OperationType.SET))
            .q0(new AttributeModification<>(50.0, OperationType.SET))
            .build();
    }

    @Override
    protected MatcherLoadModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadModificationInfos.createMatcherLoadModificationInfos((LoadModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        Load modifiedLoad = getNetwork().getLoad("v1load");
        assertNotNull(modifiedLoad);
        assertEquals(LoadType.FICTITIOUS, modifiedLoad.getLoadType());
        assertEquals(200.0, modifiedLoad.getP0(), 0.0);
        assertEquals(30.0, modifiedLoad.getQ0(), 0.0);
        assertEquals("nameLoad1", modifiedLoad.getNameOrId());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        Load modifiedLoad = getNetwork().getLoad("v1load");
        assertNotNull(modifiedLoad);
        assertEquals(LoadType.UNDEFINED, modifiedLoad.getLoadType());
        assertEquals(0.0, modifiedLoad.getP0(), 0.0);
        assertEquals(0.0, modifiedLoad.getQ0(), 0.0);
        assertEquals("v1load", modifiedLoad.getNameOrId());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // Unset an attribute that should not be null
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .id("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        String loadModificationInfosJson = mapper.writeValueAsString(loadModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadModificationInfosJson)
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().is5xxServerError(),
                        content().string(
                                new NetworkModificationException(MODIFY_LOAD_ERROR, "Load 'v1load': load type is null")
                                        .getMessage()));
    }
}
