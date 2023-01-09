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

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GroovyScriptModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherGroovyScriptModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_EMPTY;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
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
    public void testGroovy() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        GroovyScriptModificationInfos groovyScriptModificationInfos = GroovyScriptModificationInfos.builder()
                .type(ModificationType.GROOVY_SCRIPT)
                .script("network.getGenerator('idGenerator').targetP=12\n")
                .build();
        String groovyScriptModificationInfosJson = mapper.writeValueAsString(groovyScriptModificationInfos);

        // apply groovy script with generator target P modification
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrbStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with load type modification
        groovyScriptModificationInfos.setScript("network.getLoad('v1load').loadType=com.powsybl.iidm.network.LoadType.FICTITIOUS\n");
        groovyScriptModificationInfosJson = mapper.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrBranchStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with lcc converter station power factor modification
        groovyScriptModificationInfos.setScript("network.getLccConverterStation('v1lcc').powerFactor=1\n");
        groovyScriptModificationInfosJson = mapper.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmListResultBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultBranchStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with line R modification
        groovyScriptModificationInfos.setScript("network.getLine('line1').r=2\n");
        groovyScriptModificationInfosJson = mapper.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrbInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1", "s2")));

        // apply groovy script with two windings transformer ratio tap modification
        groovyScriptModificationInfos.setScript("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n");
        groovyScriptModificationInfosJson = mapper.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrbsInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbsInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with three windings transformer phase tap modification
        groovyScriptModificationInfos.setScript("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n");
        groovyScriptModificationInfosJson = mapper.writeValueAsString(groovyScriptModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> bsmlrStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrStatusInfos.get(0), MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 6);
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
