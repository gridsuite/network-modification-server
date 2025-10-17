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
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.*;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.testBranchCreationImpacts;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class TwoWindingsTransformerCreationNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
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
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connected2(true)
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(
                                    CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(2147483647).value(671.).build()
                                )
                            ).build()
                        ).applicability(SIDE1).build(),
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build()
                        ).applicability(SIDE2).build()
                    )
                )
                .connectionName1("cn201")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connected1(true)
                .connectionName2("cn202")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connected2(true)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(1)
                        .tapPosition(2)
                        .terminalRefConnectableId("v1load")
                        .terminalRefConnectableVlId("v1")
                        .regulating(false)
                        .loadTapChangingCapabilities(true)
                        .terminalRefConnectableType("LOAD")
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
                        .terminalRefConnectableId("v2load")
                        .terminalRefConnectableVlId("v2")
                        .terminalRefConnectableType("LOAD")
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
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(3.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT5").acceptableDuration(98647).value(45.).build())).build()
                        ).applicability(SIDE1).build(),
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(2.).temporaryLimits(List.of(CurrentTemporaryLimitCreationInfos.builder().name("IT10").acceptableDuration(683647).value(791.).build())).build()
                        ).applicability(SIDE2).build()
                    )
                )
                .connectionName1("cn2012")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2022")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(10)
                        .tapPosition(20)
                        .terminalRefConnectableId("v1load")
                        .terminalRefConnectableVlId("v1")
                        .regulating(false)
                        .loadTapChangingCapabilities(true)
                        .terminalRefConnectableType("LOAD")
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
                        .terminalRefConnectableId("v2load")
                        .terminalRefConnectableVlId("v2")
                        .terminalRefConnectableType("LOAD")
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
        assertEquals(PROPERTY_VALUE, getNetwork().getTwoWindingsTransformer("new2wt").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getTwoWindingsTransformer("new2wt"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
        assertEquals(0, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("new2wt")).count());
    }

    @Test
    void testCreateTwoWindingsTransformerWithRatioTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a RatioTapChanger
        RatioTapChangerCreationInfos ratioTapChangerCreationInfos = RatioTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .terminalRefConnectableVlId("v1")
                .terminalRefConnectableId("v1load")
                .terminalRefConnectableType("LOAD")
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
                .ratioTapChanger(ratioTapChangerCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos, 1);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithRatioTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos2, 2);

        assertEquals(
            "TwoWindingsTransformerCreationInfos(super=BranchCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, type=TWO_WINDINGS_TRANSFORMER_CREATION, date=null, stashed=false, messageType=null, messageValues=null, activated=true), equipmentId=id2wt1WithRatioTapChanger2, properties=null), equipmentName=2wtName), r=400.0, x=300.0, voltageLevelId1=v1, voltageLevelId2=v4, busOrBusbarSectionId1=1.1, busOrBusbarSectionId2=1.A, operationalLimitsGroups=null, selectedOperationalLimitsGroup1=null, selectedOperationalLimitsGroup2=null, connectionName1=null, connectionDirection1=TOP, connectionName2=null, connectionDirection2=TOP, connectionPosition1=null, connectionPosition2=null, connected1=true, connected2=true), g=100.0, b=200.0, ratedU1=1000.0, ratedU2=1010.0, ratedS=null, ratioTapChanger=RatioTapChangerCreationInfos(super=TapChangerCreationInfos(lowTapPosition=0, tapPosition=1, regulating=true, targetDeadband=null, terminalRefConnectableId=v1load, terminalRefConnectableType=LOAD, terminalRefConnectableVlId=v1, steps=[TapChangerStepCreationInfos(index=0, rho=1.0, r=39.78473, x=39.784725, g=0.0, b=0.0, alpha=0.0), TapChangerStepCreationInfos(index=0, rho=1.0, r=39.78474, x=39.784726, g=0.0, b=0.0, alpha=0.0), TapChangerStepCreationInfos(index=0, rho=1.0, r=39.78475, x=39.784727, g=0.0, b=0.0, alpha=0.0)], loadTapChangingCapabilities=true), targetV=220.0), phaseTapChanger=null)",
            twoWindingsTransformerCreationInfos2.toString()
        );

        // create twt with ratioTapChanger having a null targetV
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithRatioTapChanger3")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .ratioTapChanger(RatioTapChangerCreationInfos.builder()
                        .lowTapPosition(0)
                        .tapPosition(1)
                        .regulating(false)
                        .targetDeadband(null)
                        .terminalRefConnectableVlId("v1")
                        .terminalRefConnectableId("v1load")
                        .terminalRefConnectableType("LOAD")
                        .loadTapChangingCapabilities(true)
                        .targetV(null)
                        .steps(getTapChangerSteps())
                        .build())
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos3, 3);

    }

    @Test
    void testCreateTwoWindingsTransformerWithPhaseTapChangerInNodeBreaker() throws Exception {
        // create new 2wt in voltage level with Node/breaker topology, having a PhaseTapChanger with Load regulating
        PhaseTapChangerCreationInfos phaseTapChangerLoadRegulatingCreationInfos = PhaseTapChangerCreationInfos.builder()
                .lowTapPosition(0)
                .tapPosition(1)
                .regulating(true)
                .targetDeadband(null)
                .regulationMode(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL)
                .regulationValue(10.0)
                .terminalRefConnectableVlId("v1")
                .terminalRefConnectableId("v1load")
                .terminalRefConnectableType("LOAD")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
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
                .terminalRefConnectableVlId("v3")
                .terminalRefConnectableId("v3Battery")
                .terminalRefConnectableType("BATTERY")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos2 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger2")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
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
                .terminalRefConnectableVlId("v3")
                .terminalRefConnectableId("v2shunt")
                .terminalRefConnectableType("SHUNT_COMPENSATOR")
                .steps(getTapChangerSteps())
                .build();
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos3 = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1WithPhaseTapChanger3")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v4")
                .busOrBusbarSectionId2("1.A")
                .connected2(true)
                .g(100.0)
                .b(200.0)
                .ratedU1(1000)
                .ratedU2(1010)
                .x(300)
                .r(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(phaseTapChangerShuntCompensatorRegulatingCreationInfos)
                .build();
        testCreateTwoWindingsTransformerInNodeBreaker(twoWindingsTransformerCreationInfos3, 3);
    }

    @Test
    void testCreateWithErrors() throws Exception {
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("");
        String twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", ERROR_MESSAGE_KEY, reportService);
        testNetworkModificationsCount(getGroupId(), 1);

        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
        testNetworkModificationsCount(getGroupId(), 2);

        // Test create transformer on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the transformer cannot be created
        twoWindingsTransformerCreationInfos.setEquipmentId("id2wt3");
        twoWindingsTransformerCreationInfos.setEquipmentName("name2wt3");
        twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, "variant_not_existing");
        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        MvcResult mvcResult = mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk()).andReturn();
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(networkModificationsResult);
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().getFirst().isEmpty()); // no modifications returned
        assertNull(getNetwork().getTwoWindingsTransformer("id2wt3"));  // transformer was not created
        testNetworkModificationsCount(getGroupId(), 3);

        // try to create an existing equipment
        twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) buildModification();
        twoWindingsTransformerCreationInfos.setEquipmentId("trf1");
        twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS, "trf1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
        testNetworkModificationsCount(getGroupId(), 4);
    }

    private void testCreateTwoWindingsTransformerInNodeBreaker(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, int actualSize) throws Exception {
        MvcResult mvcResult;
        final String transformerId = twoWindingsTransformerCreationInfos.getEquipmentId();

        String twoWindingsTransformerCreationInfosJson = getJsonBody(twoWindingsTransformerCreationInfos, null);
        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(twoWindingsTransformerCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mvcResult = mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk()).andReturn();
        TwoWindingsTransformer twoWindingsTransformer = getNetwork().getTwoWindingsTransformer(transformerId);
        assertNotNull(twoWindingsTransformer);  // transformer was created
        int disconnector1Node1 = twoWindingsTransformer.getTerminal1().getNodeBreakerView().getNode() + 1;
        int disconnector2Node1 = twoWindingsTransformer.getTerminal2().getNodeBreakerView().getNode() + 1;
        testBranchCreationImpacts(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.TWO_WINDINGS_TRANSFORMER, transformerId,
            twoWindingsTransformerCreationInfos.getEquipmentId() + "1_BREAKER", twoWindingsTransformerCreationInfos.getEquipmentId() + "1_DISCONNECTOR_" + disconnector1Node1 + "_0", "s1",
            twoWindingsTransformerCreationInfos.getEquipmentId() + "2_BREAKER", twoWindingsTransformerCreationInfos.getEquipmentId() + "2_DISCONNECTOR_" + disconnector2Node1 + "_0", "s1"
        );
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

