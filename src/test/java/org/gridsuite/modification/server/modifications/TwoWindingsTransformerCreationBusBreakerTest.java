/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.PhaseTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.RatioTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.TapChangerStepCreationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.Impacts.TestImpactUtils.testElementCreationImpact;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class TwoWindingsTransformerCreationBusBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("new2wt")
                .equipmentName("new2wt")
                .seriesResistance(1.)
                .seriesReactance(2.)
                .magnetizingConductance(3.)
                .magnetizingSusceptance(4.)
                .ratedVoltage1(5.)
                .ratedVoltage2(6.)
                .ratedS(1.)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(98647).value(45.).build())).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build())
                .connectionName1("cn201")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn202")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(1)
                        .tapPosition(2)
                        .regulatingTerminalId("idGenerator1")
                        .regulatingTerminalVlId("v1")
                        .regulating(false)
                        .regulatingTerminalType("GENERATOR")
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
                        .regulatingTerminalId("idGenerator1")
                        .regulatingTerminalVlId("v2")
                        .regulatingTerminalType("GENERATOR")
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
                .equipmentId("new2wtUpdate")
                .equipmentName("new2wtUpdate")
                .seriesResistance(2.3)
                .seriesReactance(3.2)
                .magnetizingConductance(4.4)
                .magnetizingSusceptance(5.5)
                .ratedVoltage1(6.8)
                .ratedVoltage2(7.9)
                .ratedS(9.4)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus3")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(98647).value(45.).build())).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build())
                .connectionName1("cn2012")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2022")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(1)
                        .tapPosition(2)
                        .regulatingTerminalId("v1load")
                        .regulatingTerminalVlId("v1")
                        .regulating(false)
                        .regulatingTerminalType("LOAD")
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
                        .regulatingTerminalId("v2load")
                        .regulatingTerminalVlId("v2")
                        .regulatingTerminalType("LOAD")
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

    @Test
    public void testCreateWithErrors() throws Exception {
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("");
        String twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", twoWindingsTransformerCreationInfos.getErrorType().name(), reportService);

        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                twoWindingsTransformerCreationInfos.getErrorType().name(), reportService);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v12").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals("v1", getNetwork().getTwoWindingsTransformer("new2wt").getTerminal1().getVoltageLevel().getId());
        assertEquals("v12", getNetwork().getTwoWindingsTransformer("new2wt").getTerminal2().getVoltageLevel().getId());
        assertEquals(2., getNetwork().getTwoWindingsTransformer("new2wt").getX(), 0.1);
        assertEquals(5., getNetwork().getTwoWindingsTransformer("new2wt").getRatedU1(), 0.1);
        assertEquals(1, getNetwork().getTwoWindingsTransformer("new2wt").getRatedS(), 0.1);
        assertEquals(4, getNetwork().getTwoWindingsTransformer("new2wt").getRatioTapChanger().getStepCount());
        assertEquals(3, getNetwork().getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getStepCount());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, getNetwork().getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getRegulationMode());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
    }

    @Test
    public void testCreateTwoWindingsTransformerWithRatioTapChangerInBusBreaker() throws Exception {
        // create new 2wt in voltage level with Bus/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationInfos ratioTapChangerCreationInfos = RatioTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .loadTapChangingCapabilities(true)
                .targetV(220.)
                .steps(getTapChangerSteps())
                .build();
        // create new 2wt in voltage level with bus/breaker topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .ratedS(200.)
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInBusBreaker(twoWindingsTransformerCreationInfos, 1);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithRatioTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInBusBreaker(twoWindingsTransformerCreationInfos2, 2);
    }

    @Test
    public void testCreateTwoWindingsTransformerWithPhaseTapChangerInBusBreaker() throws Exception {
        // create new 2wt in voltage level with Bus/breaker topology, having a PhaseTapChanger
        PhaseTapChangerCreationInfos phaseTapChangerCreationInfos = PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(phaseTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInBusBreaker(twoWindingsTransformerCreationInfos, 1);
    }

    private void testCreateTwoWindingsTransformerInBusBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, int actualSize) throws Exception {
        MvcResult mvcResult;
        final String transformerId = twoWindingsTransformerCreationInfos.getEquipmentId();

        String twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testElementCreationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.TWO_WINDINGS_TRANSFORMER, twoWindingsTransformerCreationInfos.getEquipmentId(), Set.of("s1"));
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

