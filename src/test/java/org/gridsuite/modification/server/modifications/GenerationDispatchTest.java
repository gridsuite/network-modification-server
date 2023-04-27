/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.GenerationDispatchInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherGenerationDispatchInfos;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GenerationDispatchTest extends AbstractNetworkModificationTest {
    private static final String GH1_ID = "GH1";
    private static final String GH2_ID = "GH2";
    private static final String GH3_ID = "GH3";
    private static final String GTH1_ID = "GTH1";
    private static final String GTH2_ID = "GTH2";
    private static final String TEST1_ID = "TEST1";
    private static final String GROUP1_ID = "GROUP1";
    private static final String GROUP2_ID = "GROUP2";
    private static final String GROUP3_ID = "GROUP3";
    private static final String ABC_ID = "ABC";

    @SneakyThrows
    @Test
    public void testPartialGenerationDispatch() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(90.);

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modification)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm"));
    }

    @Override
    protected ModificationInfos buildModification() {
        return GenerationDispatchInfos.builder()
            .lossCoefficient(20.)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GenerationDispatchInfos.builder()
            .lossCoefficient(50.)
            .build();
    }

    @Override
    protected MatcherGenerationDispatchInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGenerationDispatchInfos.createMatcherGenerationDispatchInfos((GenerationDispatchInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(128., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(0., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(0., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(85.357, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(90.0, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(155.714, getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100, getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(250.9944, getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(24.0, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(85.357, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
    }
}
