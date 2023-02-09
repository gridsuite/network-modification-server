/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherTwoWindingsTransformerCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class TwoWindingsTransformerCreationMixedBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createMixedTopology(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerCreationInfos.builder()
            .id("id2wt1")
            .name("2wtName")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v3")
            .busOrBusbarSectionId2("bus3")
            .g(100.0)
            .b(200.0)
            .ratedU1(1000)
            .ratedU2(1010)
            .ratedS(1.)
            .x(300)
            .r(400)
            .position1(ConnectablePositionInfos.builder()
                .label("cnid2wt1")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cnid2wt2")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(1)
                .tapPosition(2)
                .regulatingTerminal(RegulatingTerminalInfos.builder()
                    .id("v1load")
                    .vlId("v1")
                    .type("LOAD").build())
                .regulating(false)
                .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(1)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .alpha(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(2)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .alpha(0.)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(3)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .alpha(0.)
                        .build()
                )).build())
            .ratioTapChanger(RatioTapChangerCreationInfos.builder()
                .lowTapPosition(5)
                .tapPosition(6)
                .regulating(true)
                .targetDeadband(1.)
                .regulatingTerminal(RegulatingTerminalInfos.builder()
                    .id("v1load")
                    .vlId("v1")
                    .type("LOAD").build())
                .loadTapChangingCapabilities(true)
                .targetV(5.)
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(5)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(6)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(7)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(8)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build()
                ))
                .build())
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return TwoWindingsTransformerCreationInfos.builder()
            .id("new2wtUpdate")
            .name("new2wtUpdate")
            .r(2.3)
            .x(3.2)
            .g(4.4)
            .b(5.5)
            .ratedU1(6.8)
            .ratedU2(7.9)
            .ratedS(9.4)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus3")
            .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).build())
            .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).build())
            .position1(ConnectablePositionInfos.builder()
                .label("cn2012")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn2022")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(1)
                .tapPosition(2)
                .regulatingTerminal(RegulatingTerminalInfos.builder()
                    .id("v1load")
                    .vlId("v1")
                    .type("LOAD").build())
                .regulating(false)
                .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(1)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .alpha(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(2)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .alpha(0.)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(3)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .alpha(0.)
                        .build()
                )).build())
            .ratioTapChanger(RatioTapChangerCreationInfos.builder()
                .lowTapPosition(5)
                .tapPosition(6)
                .regulating(true)
                .targetDeadband(1.)
                .regulatingTerminal(RegulatingTerminalInfos.builder()
                    .id("v1load")
                    .vlId("v1")
                    .type("LOAD").build())
                .loadTapChangingCapabilities(true)
                .targetV(5.)
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(5)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(6)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(7)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(8)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(0)
                        .b(0)
                        .build()
                ))
                .build())
            .build();
    }

    @Override
    protected MatcherTwoWindingsTransformerCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos((TwoWindingsTransformerCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getTwoWindingsTransformer("id2wt1"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("id2wt1")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v3").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("id2wt1")).count());
        assertEquals("v1", getNetwork().getTwoWindingsTransformer("id2wt1").getTerminal1().getVoltageLevel().getId());
        assertEquals("v3", getNetwork().getTwoWindingsTransformer("id2wt1").getTerminal2().getVoltageLevel().getId());
        assertEquals(300., getNetwork().getTwoWindingsTransformer("id2wt1").getX(), 0.1);
        assertEquals(1000, getNetwork().getTwoWindingsTransformer("id2wt1").getRatedU1(), 0.1);
        assertEquals(1., getNetwork().getTwoWindingsTransformer("id2wt1").getRatedS(), 0.1);
        assertEquals(4, getNetwork().getTwoWindingsTransformer("id2wt1").getRatioTapChanger().getStepCount());
        assertEquals(3, getNetwork().getTwoWindingsTransformer("id2wt1").getPhaseTapChanger().getStepCount());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, getNetwork().getTwoWindingsTransformer("id2wt1").getPhaseTapChanger().getRegulationMode());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getTwoWindingsTransformer("id2wt1"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("id2wt1")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v3").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("id2wt1")).count());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setId("");
        String twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NetworkModificationException.Type.CREATE_TWO_WINDINGS_TRANSFORMER_ERROR, "Invalid id ''").getMessage());

        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isNotFound(), content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage()));
    }
}

