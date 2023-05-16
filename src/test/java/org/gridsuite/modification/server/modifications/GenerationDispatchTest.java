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

import java.util.UUID;

import static org.junit.Assert.assertEquals;

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
    private static final String NEW_GROUP1_ID = "newGroup1";
    private static final String NEW_GROUP2_ID = "newGroup2";

    @SneakyThrows
    @Test
    public void testGenerationDispatch() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(20.);

        // network with 2 synchronous components and 2 hvdc lines between them
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));
        GenerationDispatch generationDispatch = new GenerationDispatch((GenerationDispatchInfos) modification);
        generationDispatch.apply(getNetwork());

        assertNetworkAfterCreationWithStandardLossCoefficient();

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertEquals(528., generationDispatch.getTotalDemand(firstSynchronousComponentNum), 0.001);
        assertEquals(90., generationDispatch.getHvdcBalance(firstSynchronousComponentNum), 0.001);
        assertEquals(138., generationDispatch.getRemainigPowerImbalance(firstSynchronousComponentNum), 0.001); // supply-demand balance could not be met on first synchronous component

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertEquals(240., generationDispatch.getTotalDemand(secondSynchronousComponentNum), 0.001);
        assertEquals(-90., generationDispatch.getHvdcBalance(secondSynchronousComponentNum), 0.001);
        assertEquals(0., generationDispatch.getRemainigPowerImbalance(secondSynchronousComponentNum), 0.001); // supply-demand balance could be met on second synchronous component
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchWithHigherLossCoefficient() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(90.);

        // network with 2 synchronous components and 2 hvdc lines between them
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));
        GenerationDispatch generationDispatch = new GenerationDispatch((GenerationDispatchInfos) modification);
        generationDispatch.apply(getNetwork());

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
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertEquals(836., generationDispatch.getTotalDemand(firstSynchronousComponentNum), 0.001);
        assertEquals(90., generationDispatch.getHvdcBalance(firstSynchronousComponentNum), 0.001);
        assertEquals(446., generationDispatch.getRemainigPowerImbalance(firstSynchronousComponentNum), 0.001); // supply-demand balance could not be met on first synchronous component

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertEquals(380., generationDispatch.getTotalDemand(secondSynchronousComponentNum), 0.001);
        assertEquals(-90., generationDispatch.getHvdcBalance(secondSynchronousComponentNum), 0.001);
        assertEquals(70., generationDispatch.getRemainigPowerImbalance(secondSynchronousComponentNum), 0.001); // supply-demand balance could not be met on second synchronous component
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchWithInternalHvdc() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(20.);

        // network with unique synchronous component and internal hvdc lines
        setNetwork(Network.read("testGenerationDispatchInternalHvdc.xiidm", getClass().getResourceAsStream("/testGenerationDispatchInternalHvdc.xiidm")));
        GenerationDispatch generationDispatch = new GenerationDispatch((GenerationDispatchInfos) modification);
        generationDispatch.apply(getNetwork());

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
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on unique synchronous component
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in the unique synchronous component
        assertEquals(768., generationDispatch.getTotalDemand(firstSynchronousComponentNum), 0.001);
        assertEquals(0., generationDispatch.getHvdcBalance(firstSynchronousComponentNum), 0.001);
        assertEquals(68., generationDispatch.getRemainigPowerImbalance(firstSynchronousComponentNum), 0.001);  // supply-demand balance could not be met on unique synchronous component
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

    private void assertNetworkAfterCreationWithStandardLossCoefficient() {
        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(30., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNetworkAfterCreationWithStandardLossCoefficient();
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(85.357, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(24.0, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(85.357, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);
    }
}
