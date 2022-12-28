/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;

import lombok.SneakyThrows;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GroovyScriptModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherGroovyScriptModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_EMPTY;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class GroovyScriptModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=12\n")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=15\n")
                .build();
    }

    @Override
    protected MatcherGroovyScriptModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos((GroovyScriptModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(12, getNetwork().getGenerator("idGenerator").getTargetP(), 0);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(42.1, getNetwork().getGenerator("idGenerator").getTargetP(), 0);
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        GroovyScriptModificationInfos groovyScriptModificationInfos = (GroovyScriptModificationInfos) buildModification();
        groovyScriptModificationInfos.setScript("");
        // apply empty groovy script
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(groovyScriptModificationInfos))
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().isBadRequest());

        groovyScriptModificationInfos.setScript("      ");
        // apply empty groovy script
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(groovyScriptModificationInfos))
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage()));

        groovyScriptModificationInfos.setScript("network.getGenerator('there is no generator').targetP=12\n");
        // apply groovy script with unknown generator
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(groovyScriptModificationInfos))
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(GROOVY_SCRIPT_ERROR,
                                "Cannot set property 'targetP' on null object").getMessage()));
    }
}
