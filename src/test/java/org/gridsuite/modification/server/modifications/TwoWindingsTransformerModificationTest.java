/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.LoadingLimits;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TwoSides;
import com.powsybl.iidm.network.TwoWindingsTransformer;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.TwoWindingsTransformerToBeEstimated;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.utils.ModificationUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR;
import static org.gridsuite.modification.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;
import static org.gridsuite.modification.modifications.TwoWindingsTransformerModification.processPhaseTapRegulation;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.NetworkUtil.createTwoWindingsTransformer;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Tag("IntegrationTest")
class TwoWindingsTransformerModificationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerModificationInfos.builder().stashed(false).equipmentId("trf1")
                .equipmentName(new AttributeModification<>("2wt modified name", OperationType.SET))
                .r(new AttributeModification<>(1., OperationType.SET))
                .x(new AttributeModification<>(2., OperationType.SET))
                .g(new AttributeModification<>(3., OperationType.SET))
                .b(new AttributeModification<>(4., OperationType.SET))
                .ratedU1(new AttributeModification<>(5., OperationType.SET))
                .ratedU2(new AttributeModification<>(6., OperationType.SET))
                .ratedS(new AttributeModification<>(7., OperationType.SET))
                .ratioTapChangerToBeEstimated(new AttributeModification<>(true, OperationType.SET))
                .phaseTapChangerToBeEstimated(new AttributeModification<>(false, OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(12.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(null)
                                .name("name31")
                                .value(null)
                                .modificationType(TemporaryLimitModificationType.ADDED)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(32)
                                .name("name32")
                                .value(42.0)
                                .modificationType(TemporaryLimitModificationType.ADDED)
                                .build()))
                        .build())
                .voltageLevelId1(new AttributeModification<>("v1", OperationType.SET))
                .voltageLevelId2(new AttributeModification<>("v2", OperationType.SET))
                .busOrBusbarSectionId1(new AttributeModification<>("1B", OperationType.SET))
                .busOrBusbarSectionId2(new AttributeModification<>("2B", OperationType.SET))
                .connectionName1(new AttributeModification<>("trf1", OperationType.SET))
                .connectionName2(new AttributeModification<>("trf1", OperationType.SET))
                .connectionDirection1(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionDirection2(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .connectionPosition1(new AttributeModification<>(1, OperationType.SET))
                .connectionPosition2(new AttributeModification<>(2, OperationType.SET))
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<>(true, OperationType.SET))
                        .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                        .regulating(new AttributeModification<>(false, OperationType.SET))
                        .targetV(new AttributeModification<>(100., OperationType.SET))
                        .targetDeadband(new AttributeModification<>(100., OperationType.SET))
                        .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                        .tapPosition(new AttributeModification<>(1, OperationType.SET))
                        .regulatingTerminalId(new AttributeModification<>("trf1", OperationType.SET))
                        .regulatingTerminalType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                        .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                .index(0)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(1)
                                .build(),
                                TapChangerStepCreationInfos.builder()
                                .index(1)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(1)
                                .build()
                                ))
                        .build())
                .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                    .enabled(new AttributeModification<>(true, OperationType.SET))
                    .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                    .regulationValue(new AttributeModification<>(100., OperationType.SET))
                    .targetDeadband(new AttributeModification<>(100., OperationType.SET))
                    .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                    .tapPosition(new AttributeModification<>(1, OperationType.SET))
                    .regulatingTerminalId(new AttributeModification<>("trf1", OperationType.SET))
                    .regulatingTerminalType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                    .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                    .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .alpha(1.)
                        .build(),
                        TapChangerStepCreationInfos.builder()
                            .index(1)
                            .r(0)
                            .g(0)
                            .b(0)
                            .x(0)
                            .rho(1)
                            .alpha(1.1)
                            .build()
                        ))
                    .build())
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return TwoWindingsTransformerModificationInfos.builder()
                .stashed(false)
                .equipmentId("trf1Edited")
                .equipmentName(new AttributeModification<>("2wt modified name again", OperationType.SET))
                .r(new AttributeModification<>(1.1, OperationType.SET))
                .x(new AttributeModification<>(2.1, OperationType.SET))
                .g(new AttributeModification<>(3.1, OperationType.SET))
                .b(new AttributeModification<>(4.1, OperationType.SET))
                .ratedU1(new AttributeModification<>(5.1, OperationType.SET))
                .ratedU2(new AttributeModification<>(6.1, OperationType.SET))
                .ratedS(new AttributeModification<>(7.1, OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(21.1)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(33)
                                .name("name33")
                                .value(41.1)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.1)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(35)
                                .name("name35")
                                .value(42.1)
                                .build()))
                        .build())
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<>(true, OperationType.SET))
                        .loadTapChangingCapabilities(new AttributeModification<>(true, OperationType.SET))
                        .regulating(new AttributeModification<>(false, OperationType.SET))
                        .targetV(new AttributeModification<>(100., OperationType.SET))
                        .targetDeadband(new AttributeModification<>(100., OperationType.SET))
                        .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                        .tapPosition(new AttributeModification<>(1, OperationType.SET))
                        .regulatingTerminalId(new AttributeModification<>("trf1", OperationType.SET))
                        .regulatingTerminalType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                        .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                .index(0)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(1)
                                .build(),
                                TapChangerStepCreationInfos.builder()
                                .index(1)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(3)
                                .build()
                                ))
                        .build())
                .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                    .enabled(new AttributeModification<>(true, OperationType.SET))
                    .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET))
                    .regulationValue(new AttributeModification<>(100.1, OperationType.SET))
                    .targetDeadband(new AttributeModification<>(100.1, OperationType.SET))
                    .lowTapPosition(new AttributeModification<>(1, OperationType.SET))
                    .tapPosition(new AttributeModification<>(1, OperationType.SET))
                    .regulatingTerminalId(new AttributeModification<>("trf1", OperationType.SET))
                    .regulatingTerminalType(new AttributeModification<>("TWO_WINDINGS_TRANSFORMER", OperationType.SET))
                    .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                    .steps(List.of(TapChangerStepCreationInfos.builder()
                            .index(0)
                            .r(0)
                            .g(0)
                            .b(0)
                            .x(0)
                            .rho(1)
                            .alpha(1.2)
                            .build(),
                        TapChangerStepCreationInfos.builder()
                            .index(1)
                            .r(0)
                            .g(0)
                            .b(0)
                            .x(0)
                            .rho(1)
                            .alpha(1.3)
                            .build()
                    )).build())
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TwoWindingsTransformer modifiedTwoWindingsTransformer = getNetwork().getTwoWindingsTransformer("trf1");
        assertNotNull(modifiedTwoWindingsTransformer);
        assertEquals("2wt modified name", modifiedTwoWindingsTransformer.getNameOrId());
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("trf1")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("trf1")).count());
        assertEquals("v1", modifiedTwoWindingsTransformer.getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", modifiedTwoWindingsTransformer.getTerminal2().getVoltageLevel().getId());
        assertEquals(1.0, modifiedTwoWindingsTransformer.getR(), 0.1);
        assertEquals(2.0, modifiedTwoWindingsTransformer.getX(), 0.1);
        assertEquals(3.0, modifiedTwoWindingsTransformer.getG(), 0.1);
        assertEquals(4.0, modifiedTwoWindingsTransformer.getB(), 0.1);
        assertEquals(5.0, modifiedTwoWindingsTransformer.getRatedU1(), 0.1);
        assertEquals(6.0, modifiedTwoWindingsTransformer.getRatedU2(), 0.1);
        assertEquals(7.0, modifiedTwoWindingsTransformer.getRatedS(), 0.1);
        // limits
        assertNotNull(modifiedTwoWindingsTransformer.getNullableCurrentLimits1());
        assertEquals(12.0, modifiedTwoWindingsTransformer.getNullableCurrentLimits1().getPermanentLimit());
        LoadingLimits.TemporaryLimit temporaryLimit = modifiedTwoWindingsTransformer.getNullableCurrentLimits1().getTemporaryLimit(Integer.MAX_VALUE);
        assertEquals(Integer.MAX_VALUE, temporaryLimit.getAcceptableDuration());
        assertEquals("name31", temporaryLimit.getName());
        assertEquals(Double.MAX_VALUE, temporaryLimit.getValue());
        assertNotNull(modifiedTwoWindingsTransformer.getNullableCurrentLimits2());
        assertEquals(22.0, modifiedTwoWindingsTransformer.getNullableCurrentLimits2().getPermanentLimit());
        temporaryLimit = modifiedTwoWindingsTransformer.getNullableCurrentLimits2().getTemporaryLimit(32);
        assertEquals(32, temporaryLimit.getAcceptableDuration());
        assertEquals("name32", temporaryLimit.getName());
        assertEquals(42.0, temporaryLimit.getValue());
        assertEquals(PROPERTY_VALUE, modifiedTwoWindingsTransformer.getProperty(PROPERTY_NAME));
        // toBeEstimated extension
        TwoWindingsTransformerToBeEstimated toBeEstimated = modifiedTwoWindingsTransformer.getExtension(TwoWindingsTransformerToBeEstimated.class);
        assertNotNull(toBeEstimated);
        assertTrue(toBeEstimated.shouldEstimateRatioTapChanger());
        assertFalse(toBeEstimated.shouldEstimatePhaseTapChanger());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        TwoWindingsTransformer modifiedTwoWindingsTransformer = getNetwork().getTwoWindingsTransformer("trf1");
        assertNotNull(modifiedTwoWindingsTransformer);
        assertEquals("trf1", modifiedTwoWindingsTransformer.getNameOrId());
        assertEquals(1, getNetwork().getVoltageLevel("v1").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("trf1")).count());
        assertEquals(1, getNetwork().getVoltageLevel("v2").getTwoWindingsTransformerStream().filter(transformer -> transformer.getId().equals("trf1")).count());
        assertEquals("v1", modifiedTwoWindingsTransformer.getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", modifiedTwoWindingsTransformer.getTerminal2().getVoltageLevel().getId());
        assertEquals(2.0, modifiedTwoWindingsTransformer.getR(), 0.1);
        assertEquals(14.745, modifiedTwoWindingsTransformer.getX(), 0.1);
        assertEquals(0.0, modifiedTwoWindingsTransformer.getG(), 0.1);
        assertEquals(3.2E-5, modifiedTwoWindingsTransformer.getB(), 0.1);
        assertEquals(400.0, modifiedTwoWindingsTransformer.getRatedU1(), 0.1);
        assertEquals(225.0, modifiedTwoWindingsTransformer.getRatedU2(), 0.1);
        // limits
        assertNull(modifiedTwoWindingsTransformer.getNullableCurrentLimits1());
        assertNull(modifiedTwoWindingsTransformer.getNullableCurrentLimits2());
    }

    @Test
    void testCreateWithErrors() throws Exception {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) buildModification();
        twoWindingsTransformerModificationInfos.setEquipmentId("2wt_not_existing");
        String modificationInfosJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationInfosJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_NOT_FOUND, "Two windings transformer '2wt_not_existing' : it does not exist in the network").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testRatioTapChangerModification() throws Exception {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = TwoWindingsTransformerModificationInfos.builder()
                .stashed(false)
                .equipmentId("trf1")
                .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                        .build())
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<Boolean>(true, OperationType.SET))
                        .regulating(new AttributeModification<Boolean>(true, OperationType.SET))
                        .build())
                .build();

        String modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        TwoWindingsTransformerModificationInfos createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //set regulating to false and unset target deadband on the twtTransformer to modify
        TwoWindingsTransformer twtToModify = getNetwork().getTwoWindingsTransformer("trf1");
        twtToModify.getRatioTapChanger().setRegulating(false);
        twtToModify.getRatioTapChanger().setTargetDeadband(Double.NaN);
        //unset regulating and modify target voltage
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulating(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetV(new AttributeModification<>(250.0, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset target voltage and modify regulating terminal
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulating(new AttributeModification<>(true, OperationType.SET));
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetV(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulatingTerminalId(new AttributeModification<>("trf1_terminal1", OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset regulating terminal and modify deadband
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulatingTerminalId(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetDeadband(new AttributeModification<>(22.0, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset deadband and modify tap position
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetDeadband(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTapPosition(new AttributeModification<>(0, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset tap position and modify steps
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTapPosition(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setSteps(List.of(TapChangerStepCreationInfos.builder()
                                .index(0)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(1)
                                .build(),
                                TapChangerStepCreationInfos.builder()
                                .index(1)
                                .r(0)
                                .g(0)
                                .b(0)
                                .x(0)
                                .rho(3)
                                .build()
                                ));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //disable the tap changer
        twoWindingsTransformerModificationInfos.setRatioTapChanger(RatioTapChangerModificationInfos.builder().enabled(new AttributeModification<>(false, OperationType.SET)).build());

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(6);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

    }

    @Test
    void testPhaseTapChangerModification() throws Exception {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = TwoWindingsTransformerModificationInfos.builder()
                .stashed(false)
                .equipmentId("trf2")
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .build())
                .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<>(true, OperationType.SET))
                        .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                        .regulationValue(new AttributeModification<>(100.0, OperationType.SET))
                        .targetDeadband(new AttributeModification<>(10.0, OperationType.SET))
                        .build())
                .build();

        String modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        TwoWindingsTransformerModificationInfos createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset regulating mode and modify regulation value
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationMode(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationValue(new AttributeModification<>(250.0, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset target voltage and modify regulating terminal
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationValue(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulatingTerminalId(new AttributeModification<>("trf1_terminal1", OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset regulating terminal and modify deadband
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulatingTerminalId(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTargetDeadband(new AttributeModification<>(22.0, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset deadband and modify tap position
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTargetDeadband(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTapPosition(new AttributeModification<>(0, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        // unset tap position and modify steps
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTapPosition(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setSteps(List.of(TapChangerStepCreationInfos.builder()
                            .index(0)
                            .r(0)
                            .g(0)
                            .b(0)
                            .x(0)
                            .rho(1)
                            .alpha(1.2)
                            .build(),
                        TapChangerStepCreationInfos.builder()
                            .index(1)
                            .r(0)
                            .g(0)
                            .b(0)
                            .x(0)
                            .rho(1)
                            .alpha(1.3)
                            .build()));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        // unset steps and modify regulation type and regulation side
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setSteps(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET));
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationSide(new AttributeModification<>(RegulationSide.SIDE1, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(6);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        // unset regulation type and modify regulation side to side 2
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationType(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationSide(new AttributeModification<>(RegulationSide.SIDE2, OperationType.SET));

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(7);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //disable the tap changer
        twoWindingsTransformerModificationInfos.setPhaseTapChanger(PhaseTapChangerModificationInfos.builder().enabled(new AttributeModification<>(false, OperationType.SET)).build());

        modificationToCreateJson = getJsonBody(twoWindingsTransformerModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(8);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

    }

    private TwoWindingsTransformer createPhaseTapChanger() {
        return createPhaseTapChanger(PhaseTapChanger.RegulationMode.CURRENT_LIMITER);
    }

    private TwoWindingsTransformer createPhaseTapChanger(PhaseTapChanger.RegulationMode regulationMode) {
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);
        Terminal phaseTapChangerTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(getNetwork(),
            "v3load",
            "LOAD",
            "v3");
        twt3.newPhaseTapChanger()
            .setLowTapPosition(0)
            .setTapPosition(1)
            .setRegulationTerminal(phaseTapChangerTerminal)
            .setRegulationMode(regulationMode)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.1)
            .endStep()
            .add();
        return twt3;
    }

    @Test
    void testPhaseTapChangerRegulationModification() throws Exception {
        TwoWindingsTransformer twt3 = createPhaseTapChanger();
        String twtId = "trf3";
        // modification 1
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .regulationValue(new AttributeModification<>(10.0, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .build())
            .build();

        String modificationToModifyJson = getJsonBody(phaseTapChangerCreation, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToModifyJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // modification 1 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());

        // modification 2
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationValue(null);
        phaseTapChangerCreation.getPhaseTapChanger().setRegulating(new AttributeModification<>(false, OperationType.SET));

        String modificationToModifyJson2 = getJsonBody(phaseTapChangerCreation, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToModifyJson2).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertFalse(phaseTapChanger.isRegulating());

        // modification 3
        phaseTapChangerCreation.getPhaseTapChanger().setRegulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setTargetDeadband(new AttributeModification<>(1.0, OperationType.SET));
        phaseTapChangerCreation.getPhaseTapChanger().setRegulating(new AttributeModification<>(true, OperationType.SET));

        String modificationToModifyJson3 = getJsonBody(phaseTapChangerCreation, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToModifyJson3).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        phaseTapChanger = getNetwork().getTwoWindingsTransformer(twtId).getPhaseTapChanger();

        // modification 3 assert
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(1.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertTrue(phaseTapChanger.isRegulating());
    }

    @Test
    void testPhaseTapChangerRegulationModification2() throws Exception {
        createPhaseTapChanger();
        String twtId = "trf3";

        // modification 1 error
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET))
                .build())
            .build();

        String modificationToModifyJson1 = getJsonBody(phaseTapChangerCreation, null);

        // modification 1 assert
        mockMvc.perform(post(getNetworkModificationUri())
                .content(modificationToModifyJson1).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR, "Regulation value is missing when modifying, phase tap changer can not regulate").getMessage(),
            ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("trf1", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("trf1Edited", updatedValues.get("equipmentId"));
    }

    @Test
    void testChangeConnectionStatus() throws Exception {
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.ONE, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.ONE, true, false, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, true, null);
        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, false, "Could not disconnect equipment 'trf2' on side 2");
    }

    private void changeConnectionState(TwoWindingsTransformer existingEquipment, TwoSides side, boolean actualState, boolean expectedState, String errorMessage) throws Exception {
        Terminal terminal = existingEquipment.getTerminal(side);
        assertThat(terminal.isConnected()).isEqualTo(actualState);

        TwoWindingsTransformerModificationInfos modificationInfos =
                TwoWindingsTransformerModificationInfos.builder()
                        .stashed(false)
                        .equipmentId(existingEquipment.getId())
                        .terminal1Connected(side == TwoSides.ONE ? new AttributeModification<>(expectedState, OperationType.SET) : null)
                        .terminal2Connected(side == TwoSides.TWO ? new AttributeModification<>(expectedState, OperationType.SET) : null)
                        .build();
        String modificationInfosJson = getJsonBody(modificationInfos, null);

        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());

        if (!Objects.isNull(errorMessage)) {
            // change not applied
            assertThat(terminal.isConnected()).isNotEqualTo(expectedState);
            assertEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, extractApplicationStatus(networkModificationsResult).getFirst());
            assertLogMessage("BRANCH_MODIFICATION_ERROR : " + errorMessage, ERROR_MESSAGE_KEY, reportService);
        } else {
            // connection state has changed as expected
            assertThat(terminal.isConnected()).isEqualTo(expectedState);
            assertEquals(NetworkModificationResult.ApplicationStatus.ALL_OK, extractApplicationStatus(networkModificationsResult).getFirst());

            // try to modify again => no change on connection state
            mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk());
            assertThat(terminal.isConnected()).isEqualTo(expectedState);
        }
    }

    @Test
    void testProcessPhaseTapChangerModification() {
        TwoWindingsTransformer twt = createPhaseTapChanger();
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();
        List<ReportNode> regulationReports = new ArrayList<>();
        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            null, null, null, regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertFalse(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET),
            new AttributeModification<>(10.0, OperationType.SET), null,
            new AttributeModification<>(true, OperationType.SET), regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertTrue(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, OperationType.SET),
            null, null,
            new AttributeModification<>(false, OperationType.SET), regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.ACTIVE_POWER_CONTROL, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertFalse(phaseTapChanger.isRegulating());

        processPhaseTapRegulation(phaseTapChanger, null, true,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            new AttributeModification<>(12.0, OperationType.SET),
            new AttributeModification<>(8.0, OperationType.SET),
            null,
            regulationReports);
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(12.0, phaseTapChanger.getRegulationValue());
        assertEquals(8.0, phaseTapChanger.getTargetDeadband());
        assertFalse(phaseTapChanger.isRegulating());
    }

    @Test
    void testProcessPhaseTapChangerCreation() {
        TwoWindingsTransformer twt = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);
        List<ReportNode> regulationReports = new ArrayList<>();
        PhaseTapChangerAdder adder = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder);
        String message = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
            null, new AttributeModification<>(10.0, OperationType.SET), null, new AttributeModification<>(true, OperationType.SET), regulationReports)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Regulation mode is missing when creating tap phase changer with regulation enabled", message);

        AttributeModification<PhaseTapChanger.RegulationMode> regulationModeModification = new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET);
        String message2 = assertThrows(NetworkModificationException.class, () -> processPhaseTapRegulation(null, adder, false,
            regulationModeModification, null, null, new AttributeModification<>(true, OperationType.SET), regulationReports)).getMessage();
        assertEquals("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR : Regulation value is missing when creating tap phase changer with regulation enabled", message2);
        processPhaseTapRegulation(null, adder, false,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            null, null, null, regulationReports);
        adder.add();
        PhaseTapChanger phaseTapChanger = twt.getPhaseTapChanger();
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertFalse(phaseTapChanger.isRegulating());

        PhaseTapChangerAdder adder1 = twt.newPhaseTapChanger();
        preparePhaseTapChangerAdder(adder1);
        processPhaseTapRegulation(null, adder1, false,
            new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET),
            new AttributeModification<>(10.0, OperationType.SET), null,
            new AttributeModification<>(true, OperationType.SET), regulationReports);
        adder1.add();
        phaseTapChanger = twt.getPhaseTapChanger();
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertTrue(phaseTapChanger.isRegulating());
    }

    private void preparePhaseTapChangerAdder(PhaseTapChangerAdder phaseTapChangerAdder) {
        Terminal phaseTapChangerTerminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(getNetwork(),
            "v3load",
            "LOAD",
            "v3");
        phaseTapChangerAdder.setLowTapPosition(0)
            .setTapPosition(1)
            .setRegulationTerminal(phaseTapChangerTerminal)
            .beginStep()
            .setR(39.78473)
            .setX(39.784725)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.)
            .endStep()
            .beginStep()
            .setR(39.78475)
            .setX(39.784727)
            .setG(0.0)
            .setB(0.0)
            .setRho(1.0)
            .setAlpha(1.1)
            .endStep();
    }

    @Test
    void testPhaseTapChangerRegulationCreation() throws Exception {
        // test with non pre-existent phase tap changer
        String twtId = "trf3";
        TwoWindingsTransformer twt3 = createTwoWindingsTransformer(getNetwork().getSubstation("s1"), "trf3", "trf3", 2.0, 14.745, 0.0, 3.2E-5, 400.0, 225.0,
            41, 151, getNetwork().getVoltageLevel("v1").getId(), getNetwork().getVoltageLevel("v2").getId(),
            "trf3", 1, ConnectablePosition.Direction.TOP,
            "trf3", 2, ConnectablePosition.Direction.TOP);
        // creation 1 : CURRENT_LIMITER
        TwoWindingsTransformerModificationInfos phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .regulationValue(new AttributeModification<>(10.0, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(1)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build()
                ))
                .build())
            .build();

        String modificationToModifyJson = getJsonBody(phaseTapChangerCreation, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToModifyJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        PhaseTapChanger phaseTapChanger = twt3.getPhaseTapChanger();

        // creation 1 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertTrue(phaseTapChanger.isRegulating());
        assertEquals(0.0, phaseTapChanger.getTargetDeadband());
        assertEquals(10.0, phaseTapChanger.getRegulationValue());

        // creation 2 : FIXED_TAP
        twt3.getPhaseTapChanger().remove();
        phaseTapChangerCreation = TwoWindingsTransformerModificationInfos.builder()
            .stashed(false)
            .equipmentId(twtId)
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .build())
            .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .enabled(new AttributeModification<>(true, OperationType.SET))
                .regulationMode(new AttributeModification<>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                .lowTapPosition(new AttributeModification<>(0, OperationType.SET))
                .tapPosition(new AttributeModification<>(1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .steps(List.of(TapChangerStepCreationInfos.builder()
                        .index(0)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build(),
                    TapChangerStepCreationInfos.builder()
                        .index(1)
                        .r(0)
                        .g(0)
                        .b(0)
                        .x(0)
                        .rho(1)
                        .build()
                ))
                .build())
            .build();

        modificationToModifyJson = getJsonBody(phaseTapChangerCreation, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToModifyJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        phaseTapChanger = twt3.getPhaseTapChanger();

        // creation 2 assert
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, phaseTapChanger.getRegulationMode());
        assertFalse(phaseTapChanger.isRegulating());
        assertTrue(Double.isNaN(phaseTapChanger.getTargetDeadband()));
        assertTrue(Double.isNaN(phaseTapChanger.getRegulationValue()));
    }
}

