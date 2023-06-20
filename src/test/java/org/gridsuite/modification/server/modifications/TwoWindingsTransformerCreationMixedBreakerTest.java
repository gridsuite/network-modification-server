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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class TwoWindingsTransformerCreationMixedBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createMixedTopology(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v3")
                .busOrBusbarSectionId2("bus3")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .ratedS(1.)
                .seriesReactance(300)
                .seriesResistance(400)
                .connectionName1("cnid2wt1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cnid2wt2")
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
                        .regulatingTerminalId("v1load")
                        .regulatingTerminalVlId("v1")
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
                        .regulatingTerminalId("v1load")
                        .regulatingTerminalVlId("v1")
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
        assertLogMessage(new NetworkModificationException(NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage(),
                twoWindingsTransformerCreationInfos.getErrorType().name(), reportService);
    }
}

