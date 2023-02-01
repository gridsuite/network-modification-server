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
import org.gridsuite.modification.server.dto.GroovyScriptInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherGroovyScriptInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_EMPTY;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GROOVY_SCRIPT_ERROR;
import static org.gridsuite.modification.server.utils.MatcherGroovyScriptInfos.createMatcherGroovyScriptInfos;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class GroovyScriptTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GroovyScriptInfos.builder()
                .script("network.getGenerator('idGenerator').targetP=12\n")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GroovyScriptInfos.builder()
                .script("network.getGenerator('idGenerator').targetP=15\n")
                .build();
    }

    @Override
    protected MatcherGroovyScriptInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherGroovyScriptInfos((GroovyScriptInfos) modificationInfos);
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

        GroovyScriptInfos groovyScriptInfos = GroovyScriptInfos.builder()
                .script("network.getGenerator('idGenerator').targetP=12\n")
                .substationIds(Set.of("s1")) // for the matcher
                .build();
        String groovyScriptInfosJson = mapper.writeValueAsString(groovyScriptInfos);

        // apply groovy script with generator target P modification
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<GroovyScriptInfos> bsmlrbStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbStatusInfos.get(0), createMatcherGroovyScriptInfos(groovyScriptInfos));

        // apply groovy script with load type modification
        groovyScriptInfos.setScript("network.getLoad('v1load').loadType=com.powsybl.iidm.network.LoadType.FICTITIOUS\n");
        groovyScriptInfosJson = mapper.writeValueAsString(groovyScriptInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<GroovyScriptInfos> bsmlrBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrBranchStatusInfos.get(0), createMatcherGroovyScriptInfos(groovyScriptInfos));

        // apply groovy script with lcc converter station power factor modification
        groovyScriptInfos.setScript("network.getLccConverterStation('v1lcc').powerFactor=1\n");
        groovyScriptInfosJson = mapper.writeValueAsString(groovyScriptInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<GroovyScriptInfos> bsmListResultBranchStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmListResultBranchStatusInfos.get(0), createMatcherGroovyScriptInfos(groovyScriptInfos));

        // apply groovy script with line R modification
        groovyScriptInfos.setScript("network.getLine('line1').r=2\n");
        groovyScriptInfos.setSubstationIds(Set.of("s1", "s2")); // for the matcher
        groovyScriptInfosJson = mapper.writeValueAsString(groovyScriptInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<GroovyScriptInfos> bsmlrbInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbInfos.get(0), createMatcherGroovyScriptInfos(groovyScriptInfos));

        // apply groovy script with two windings transformer ratio tap modification
        groovyScriptInfos.setScript("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n");
        groovyScriptInfos.setSubstationIds(Set.of("s1")); // for the matcher
        groovyScriptInfosJson = mapper.writeValueAsString(groovyScriptInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<GroovyScriptInfos> bsmlrbsInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrbsInfos.get(0), createMatcherGroovyScriptInfos(groovyScriptInfos));

        // apply groovy script with three windings transformer phase tap modification
        groovyScriptInfos.setScript("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n");
        groovyScriptInfosJson = mapper.writeValueAsString(groovyScriptInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(groovyScriptInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
                .andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<GroovyScriptInfos> bsmlrStatusInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrStatusInfos.get(0), createMatcherGroovyScriptInfos(groovyScriptInfos));

        testNetworkModificationsCount(getGroupId(), 6);
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        GroovyScriptInfos groovyScriptInfos = (GroovyScriptInfos) buildModification();
        groovyScriptInfos.setScript("");
        // apply empty groovy script
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(groovyScriptInfos))
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage()));

        groovyScriptInfos.setScript("      ");
        // apply blank groovy script
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(groovyScriptInfos))
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage()));

        groovyScriptInfos.setScript("network.getGenerator('there is no generator').targetP=12\n");
        // apply groovy script with unknown generator
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(groovyScriptInfos))
                .contentType(MediaType.APPLICATION_JSON)).andExpectAll(
                        status().isBadRequest(),
                        content().string(new NetworkModificationException(GROOVY_SCRIPT_ERROR,
                                "Cannot set property 'targetP' on null object").getMessage()));
    }
}
