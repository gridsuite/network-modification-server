/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
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
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.Impacts.TestImpactUtils.testBranchCreationImpacts;
import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class TwoWindingsTransformerCreationNodeBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
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
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(2147483647).value(671.).build())).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build())
                .connectionName1("cn201")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn202")
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

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("new2wtUpdate")
                .equipmentName("new2wtUpdate")
                .seriesResistance(10.)
                .seriesReactance(20.)
                .magnetizingConductance(33.)
                .magnetizingSusceptance(40.)
                .ratedVoltage1(50.)
                .ratedVoltage2(60.)
                .ratedS(10.)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(98647).value(45.).build())).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build())
                .connectionName1("cn2012")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2022")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(10)
                        .tapPosition(20)
                        .regulatingTerminalId("v1load")
                        .regulatingTerminalVlId("v1")
                        .regulating(false)
                        .regulatingTerminalType("LOAD")
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
                        .regulatingTerminalId("v2load")
                        .regulatingTerminalVlId("v2")
                        .regulatingTerminalType("LOAD")
                        .loadTapChangingCapabilities(true)
                        .targetV(5.)
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                        .index(5)
                                        .rho(100)
                                        .r(2)
                                        .x(7)
                                        .g(880)
                                        .b(90)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(6)
                                        .rho(1)
                                        .r(0)
                                        .x(65)
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
    protected void assertAfterNetworkModificationCreation() {
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
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
    }

    @SneakyThrows
    @Test
    public void testCreateTwoWindingsTransformerWithRatioTapChangerInNodeBreaker() {
        // create new 2wt in voltage level with Node/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationInfos ratioTapChangerCreationInfos = RatioTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("v1load")
                .regulatingTerminalType("LOAD")
                .loadTapChangingCapabilities(true)
                .targetV(220.)
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithRatioTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos, 1);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithRatioTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2, 2);
    }

    @Test
    public void testCreateTwoWindingsTransformerWithPhaseTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Load regulating
        PhaseTapChangerCreationInfos phaseTapChangerLoadRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("v1load")
                .regulatingTerminalType("LOAD")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
                .phaseTapChanger(phaseTapChangerLoadRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos, 1);

        // create new 2wt in voltage level with Node/breaker topology, PhaseTapChanger with Battery regulating
        PhaseTapChangerCreationInfos phaseTapChangerBatteryRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .regulatingTerminalVlId("v3")
                .regulatingTerminalId("v3Battery")
                .regulatingTerminalType("BATTERY")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
                .phaseTapChanger(phaseTapChangerBatteryRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2, 2);

        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Shunt compensator regulating
        PhaseTapChangerCreationInfos phaseTapChangerShuntCompensatorRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .regulatingTerminalVlId("v3")
                .regulatingTerminalId("v2shunt")
                .regulatingTerminalType("SHUNT_COMPENSATOR")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger3")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
                .phaseTapChanger(phaseTapChangerShuntCompensatorRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos3, 3);
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("");
        String twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", twoWindingsTransformerCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 1);

        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage(),
                twoWindingsTransformerCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 2);

        // Test create transformer on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the transformer cannot be created
        twoWindingsTransformerCreationInfos.setEquipmentId("id2wt3");
        twoWindingsTransformerCreationInfos.setEquipmentName("name2wt3");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationResult.isEmpty()); // no modifications returned
        assertNull(getNetwork().getTwoWindingsTransformer("id2wt3"));  // transformer was not created
        testNetworkModificationsCount(getGroupId(), 3);

        // try to create an existing equipment
        twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("trf1");
        twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS, "trf1").getMessage(),
                twoWindingsTransformerCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 4);
    }

    private void testCreateTwoWindingsTransformerInNodeBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, int actualSize) throws Exception {
        MvcResult mvcResult;
        final String transformerId = twoWindingsTransformerCreationInfos.getEquipmentId();

        String twoWindingsTransformerCreationInfosJson = mapper.writeValueAsString(twoWindingsTransformerCreationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testBranchCreationImpacts(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.TWO_WINDINGS_TRANSFORMER, transformerId,
            twoWindingsTransformerCreationInfos.getEquipmentId() + "1_BREAKER", twoWindingsTransformerCreationInfos.getEquipmentId() + "1_DISCONNECTOR", "s1",
            twoWindingsTransformerCreationInfos.getEquipmentId() + "2_BREAKER", twoWindingsTransformerCreationInfos.getEquipmentId() + "2_DISCONNECTOR", "s1"
        );
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

