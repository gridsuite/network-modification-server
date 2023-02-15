/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
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
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class TwoWindingsTransformerCreationNodeBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerCreationInfos.builder()
            .id("new2wt")
            .name("new2wt")
            .r(1.)
            .x(2.)
            .g(3.)
            .b(4.)
            .ratedU1(5.)
            .ratedU2(6.)
            .ratedS(1.)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).build())
            .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).build())

            .position1(ConnectablePositionInfos.builder()
                .label("cn201")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn202")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(1)
                .tapPosition(2)
                .regulating(false)
                .regulatingTerminal(RegulatingTerminalInfos.builder()
                    .id("v1load")
                    .vlId("v1")
                    .type("LOAD").build())
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
                    .id("v2load")
                    .vlId("v2")
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
            .r(10.)
            .x(20.)
            .g(33.)
            .b(40.)
            .ratedU1(50.)
            .ratedU2(60.)
            .ratedS(10.)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).build())
            .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).build())

            .position1(ConnectablePositionInfos.builder()
                .label("cn2012")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn2022")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(10)
                .tapPosition(20)
                .regulating(false)

                .regulatingTerminal(RegulatingTerminalInfos.builder()
                    .id("v1load")
                    .vlId("v1")
                    .type("LOAD").build())

                .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(10)
                        .rho(10)
                        .r(20)
                        .x(30)
                        .g(40)
                        .b(50)
                        .alpha(60)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(90)
                        .rho(80)
                        .r(70)
                        .x(60)
                        .g(50)
                        .b(40)
                        .alpha(20.)
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
                    .id("v2load")
                    .vlId("v2")
                    .type("LOAD").build())

                .loadTapChangingCapabilities(true)
                .targetV(5.)
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(5)
                        .rho(100)
                        .r(02)
                        .x(07)
                        .g(880)
                        .b(90)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(6)
                        .rho(1)
                        .r(0)
                        .x(065)
                        .g(0)
                        .b(88)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(7)
                        .rho(1)
                        .r(0)
                        .x(0)
                        .g(43)
                        .b(0)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(8)
                        .rho(1)
                        .r(63)
                        .x(0)
                        .g(0)
                        .b(93)
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
        assertNotNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals("v1", getNetwork().getTwoWindingsTransformer("new2wt").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", getNetwork().getTwoWindingsTransformer("new2wt").getTerminal2().getVoltageLevel().getId());
        assertEquals(2., getNetwork().getTwoWindingsTransformer("new2wt").getX(), 0.1);
        assertEquals(5., getNetwork().getTwoWindingsTransformer("new2wt").getRatedU1(), 0.1);
        assertEquals(1, getNetwork().getTwoWindingsTransformer("new2wt").getRatedS(), 0.1);
        assertEquals(4, getNetwork().getTwoWindingsTransformer("new2wt").getRatioTapChanger().getStepCount());
        assertEquals(3, getNetwork().getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getStepCount());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, getNetwork().getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getRegulationMode());

    }

    @SneakyThrows
    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
    }

    @Test
    public void testCreateTwoWindingsTransformerWithRatioTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationInfos ratioTapChangerCreationInfos = RatioTapChangerCreationInfos.builder()
            .lowTapPosition(0)
            .tapPosition(1)
            .regulating(true)
            .targetDeadband(null)

            .regulatingTerminal(RegulatingTerminalInfos.builder()
                .id("v1load")
                .vlId("v1")
                .type("LOAD").build())

            .loadTapChangingCapabilities(true)
            .targetV(220.)
            .steps(getTapChangerSteps())
            .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
            .id("id2wt1WithRatioTapChanger")
            .name("2wtName")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(100.0)
            .b(200.0)
            .ratedU1(1000)
            .ratedU2(1010)
            .x(300)
            .r(400)
            .position1(ConnectablePositionInfos.builder()
                .label("cnid2wt1")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cnid2wt2")
                .direction(ConnectablePosition.Direction.TOP).build())
            .ratioTapChanger(ratioTapChangerCreationInfos)
            .substationIds(Set.of("s1")) // for the matcher
            .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos, 1);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
            .id("id2wt1WithRatioTapChanger2")
            .name("2wtName")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(100.0)
            .b(200.0)
            .ratedU1(1000)
            .ratedU2(1010)
            .x(300)
            .r(400)
            .position1(ConnectablePositionInfos.builder()
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .direction(ConnectablePosition.Direction.TOP).build())
            .ratioTapChanger(ratioTapChangerCreationInfos)
            .substationIds(Set.of("s1")) // for the matcher
            .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2, 2);
    }

    @Test
    public void testCreateTwoWindingsTransformerWithPhaseTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger
        PhaseTapChangerCreationInfos phaseTapChangerCreationInfos = PhaseTapChangerCreationInfos.builder()
            .lowTapPosition(0)
            .tapPosition(1)
            .regulating(true)
            .targetDeadband(null)
            .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
            .regulationValue(10.0)

            .regulatingTerminal(RegulatingTerminalInfos.builder()
                .id("v1load")
                .vlId("v1")
                .type("LOAD").build())

            .steps(getTapChangerSteps())
            .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
            .id("id2wt1WithPhaseTapChanger")
            .name("2wtName")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(100.0)
            .b(200.0)
            .ratedU1(1000)
            .ratedU2(1010)
            .x(300)
            .r(400)

            .position1(ConnectablePositionInfos.builder()
                .label("cnid2wt1")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cnid2wt2")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(phaseTapChangerCreationInfos)
            .substationIds(Set.of("s1")) // for the matcher
            .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos, 1);

        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with BATTERY type
        PhaseTapChangerCreationInfos phaseTapChangerCreationInfosLine = PhaseTapChangerCreationInfos.builder()
            .lowTapPosition(0)
            .tapPosition(1)
            .regulating(true)
            .targetDeadband(null)
            .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
            .regulationValue(10.0)

            .regulatingTerminal(RegulatingTerminalInfos.builder()
                .id("v3Battery")
                .vlId("v3")
                .type("BATTERY").build())

            .steps(getTapChangerSteps())
            .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
            .id("id2wt1WithPhaseTapChanger2")
            .name("2wtName")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(100.0)
            .b(200.0)
            .ratedU1(1000)
            .ratedU2(1010)
            .x(300)
            .r(400)
            .position1(ConnectablePositionInfos.builder()
                .label("cnid2wt1")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cnid2wt2")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(phaseTapChangerCreationInfosLine)
            .substationIds(Set.of("s1")) // for the matcher
            .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2, 2);

        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Compensator type
        PhaseTapChangerCreationInfos phaseTapChangerCreationInfosCompensator = PhaseTapChangerCreationInfos.builder()
            .lowTapPosition(0)
            .tapPosition(1)
            .regulating(true)
            .targetDeadband(null)
            .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
            .regulationValue(10.0)

            .regulatingTerminal(RegulatingTerminalInfos.builder()
                .id("v3bCompensator")
                .vlId("v3")
                .type("VOLTAGE_LEVEL").build())

            .steps(getTapChangerSteps())
            .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
            .id("id2wt1WithPhaseTapChanger3")
            .name("2wtName")
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("1A")
            .g(100.0)
            .b(200.0)
            .ratedU1(1000)
            .ratedU2(1010)
            .x(300)
            .r(400)
            .position1(ConnectablePositionInfos.builder()
                .label("cnid2wt1")
                .direction(ConnectablePosition.Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cnid2wt2")
                .direction(ConnectablePosition.Direction.TOP).build())
            .phaseTapChanger(phaseTapChangerCreationInfosLine)
            .substationIds(Set.of("s1")) // for the matcher
            .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos3, 3);
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
        testNetworkModificationsCount(getGroupId(), 1);

        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isNotFound(), content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage()));
        testNetworkModificationsCount(getGroupId(), 2);

        // Test create transformer on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the transformer cannot be created
        twoWindingsTransformerCreationInfos.setId("id2wt3");
        twoWindingsTransformerCreationInfos.setName("name2wt3");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() {
        });

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(getNetwork().getTwoWindingsTransformer("id2wt3"));  // transformer was not created
        testNetworkModificationsCount(getGroupId(), 3);
    }

    private void testCreateTwoWindingsTransformerInNodeBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, int actualSize) throws Exception {
        MvcResult mvcResult;
        String resultAsString;
        final String transformerId = twoWindingsTransformerCreationInfos.getId();

        String twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<TwoWindingsTransformerCreationInfos> bsmlrTwoWindingsTransformer = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertThat(bsmlrTwoWindingsTransformer.get(0), createMatcherTwoWindingsTransformerCreationInfos(twoWindingsTransformerCreationInfos));

        assertNotNull(getNetwork().getTwoWindingsTransformer(transformerId));  // transformer was created
        testNetworkModificationsCount(getGroupId(), actualSize);
    }

    private List<TapChangerStepCreationInfos> getTapChangerSteps() {
        return List.of(
            TapChangerStepCreationInfos.builder()
                .r(39.78473)
                .x(39.784725)
                .g(0.)
                .b(0.)
                .rho(1.)
                .build(),
            TapChangerStepCreationInfos.builder()
                .r(39.78474)
                .x(39.784726)
                .g(0.)
                .b(0.)
                .rho(1.)
                .build(),
            TapChangerStepCreationInfos.builder()
                .r(39.78475)
                .x(39.784727)
                .g(0.)
                .b(0.)
                .rho(1.)
                .build()
        );
    }
}

