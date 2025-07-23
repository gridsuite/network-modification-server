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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.testBranchCreationImpacts;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class TwoWindingsTransformerCreationBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerCreationInfos.builder()
                .stashed(false)
                .equipmentId("new2wt")
                .equipmentName("new2wt")
                .r(1.)
                .x(2.)
                .g(3.)
                .b(4.)
                .ratedU1(5.)
                .ratedU2(6.)
                .ratedS(1.)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .connected1(true)
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .connected2(true)
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(98647).value(45.).build())).build()
                        ).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                                CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build()
                        ).build()
                    )
                )
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
                        .loadTapChangingCapabilities(true)
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
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return TwoWindingsTransformerCreationInfos.builder()
                .stashed(false)
                .equipmentId("new2wtUpdate")
                .equipmentName("new2wtUpdate")
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
                .operationalLimitsGroups1(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(98647).value(45.).build())).build()
                        ).build()
                    )
                )
                .operationalLimitsGroups2(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build()
                        ).build()
                    )
                )
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
                        .loadTapChangingCapabilities(true)
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
    void testCreateWithErrors() throws Exception {
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("");
        String twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", ERROR_MESSAGE_KEY, reportService);

        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
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
        assertEquals(PROPERTY_VALUE, getNetwork().getTwoWindingsTransformer("new2wt").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
    }

    @Test
    void testCreateTwoWindingsTransformerWithRatioTapChangerInBusBreaker() throws Exception {
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
                .stashed(false)
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .connected1(true)
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .ratedS(200.)
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInBusBreaker(twoWindingsTransformerCreationInfos, 1);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
                .stashed(false)
                .equipmentId("id2wt1WithRatioTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .connected1(true)
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInBusBreaker(twoWindingsTransformerCreationInfos2, 2);
    }

    @Test
    void testCreateTwoWindingsTransformerWithPhaseTapChangerInBusBreaker() throws Exception {
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
                .stashed(false)
                .equipmentId("id2wt1WithPhaseTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connected1(true)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connected2(true)
                .phaseTapChanger(phaseTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInBusBreaker(twoWindingsTransformerCreationInfos, 1);
    }

    private void testCreateTwoWindingsTransformerInBusBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, int actualSize) throws Exception {
        MvcResult mvcResult;
        final String transformerId = twoWindingsTransformerCreationInfos.getEquipmentId();

        String twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        testBranchCreationImpacts(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));
        assertNotNull(getNetwork().getTwoWindingsTransformer(transformerId));  // transformer was created
        testNetworkModificationsCount(getGroupId(), actualSize);
    }

    private static List<TapChangerStepCreationInfos> getTapChangerSteps() {
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

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("new2wt", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("new2wtUpdate", updatedValues.get("equipmentId"));
    }
}

