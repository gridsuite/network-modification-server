/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.LoadingLimits;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TwoSides;
import com.powsybl.iidm.network.TwoWindingsTransformer;

import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Tag("IntegrationTest")
public class TwoWindingsTransformerModificationTest extends AbstractNetworkModificationTest {
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
        assertEquals(PROPERTY_VALUE, getNetwork().getTwoWindingsTransformer("trf1").getProperty(PROPERTY_NAME));
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
    public void testCreateWithErrors() throws Exception {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) buildModification();
        twoWindingsTransformerModificationInfos.setEquipmentId("2wt_not_existing");
        String modificationInfosJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationInfosJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_NOT_FOUND, "Two windings transformer with ID '2wt_not_existing' does not exist in the network").getMessage(),
                twoWindingsTransformerModificationInfos.getErrorType().name(), reportService);
    }

    @Test
    public void testRatioTapChangerModification() throws Exception {
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

        String modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

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

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset target voltage and modify regulating terminal
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulating(new AttributeModification<>(true, OperationType.SET));
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetV(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulatingTerminalId(new AttributeModification<>("trf1_terminal1", OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset regulating terminal and modify deadband
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setRegulatingTerminalId(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetDeadband(new AttributeModification<>(22.0, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset deadband and modify tap position
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTargetDeadband(null);
        twoWindingsTransformerModificationInfos.getRatioTapChanger().setTapPosition(new AttributeModification<>(0, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

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

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //disable the tap changer
        twoWindingsTransformerModificationInfos.setRatioTapChanger(RatioTapChangerModificationInfos.builder().enabled(new AttributeModification<>(false, OperationType.SET)).build());

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(6);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

    }

    @Test
    public void testPhaseTapChangerModification() throws Exception {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = TwoWindingsTransformerModificationInfos.builder()
                .stashed(false)
                .equipmentId("trf2")
                .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                        .build())
                .phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                        .enabled(new AttributeModification<Boolean>(true, OperationType.SET))
                        .regulationMode(new AttributeModification<PhaseTapChanger.RegulationMode>(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, OperationType.SET))
                        .regulationValue(new AttributeModification<Double>(100.0, OperationType.SET))
                        .targetDeadband(new AttributeModification<Double>(10.0, OperationType.SET))
                        .build())
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        TwoWindingsTransformerModificationInfos createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset regulating mode and modify regulation value
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationMode(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationValue(new AttributeModification<>(250.0, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset target voltage and modify regulating terminal
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationValue(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulatingTerminalId(new AttributeModification<>("trf1_terminal1", OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset regulating terminal and modify deadband
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulatingTerminalId(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTargetDeadband(new AttributeModification<>(22.0, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //unset deadband and modify tap position
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTargetDeadband(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setTapPosition(new AttributeModification<>(0, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

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

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        // unset steps and modify regulation type and regulation side
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setSteps(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET));
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationSide(new AttributeModification<>(RegulationSide.SIDE1, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(6);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        // unset regulation type and modify regulation side to side 2
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationType(null);
        twoWindingsTransformerModificationInfos.getPhaseTapChanger().setRegulationSide(new AttributeModification<>(RegulationSide.SIDE2, OperationType.SET));

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(7);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

        //disable the tap changer
        twoWindingsTransformerModificationInfos.setPhaseTapChanger(PhaseTapChangerModificationInfos.builder().enabled(new AttributeModification<>(false, OperationType.SET)).build());

        modificationToCreateJson = mapper.writeValueAsString(twoWindingsTransformerModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        createdModification = (TwoWindingsTransformerModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(8);

        assertThat(createdModification).recursivelyEquals(twoWindingsTransformerModificationInfos);

    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("trf1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TWO_WINDINGS_TRANSFORMER_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("trf1Edited", updatedValues.get("equipmentId"));
    }

    @Test
    @SneakyThrows
    public void testChangeConnectionStatus() {
//        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.ONE, true, false, "Could not disconnect equipment 'trf1' on side ONE");
//        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf1"), TwoSides.TWO, true, false, "Could not disconnect equipment 'trf1' on side TWO");
//        changeConnectionState(getNetwork().getTwoWindingsTransformer("trf2"), TwoSides.TWO, true, true, null);
    }

    @SneakyThrows
    private void changeConnectionState(TwoWindingsTransformer existingEquipment, TwoSides side, boolean actualState, boolean expectedState, String errorMessage) {
        Terminal terminal = existingEquipment.getTerminal(side);
        assertThat(terminal.isConnected()).isEqualTo(actualState);

        TwoWindingsTransformerModificationInfos modificationInfos =
                TwoWindingsTransformerModificationInfos.builder()
                        .stashed(false)
                        .equipmentId(existingEquipment.getId())
                        .connected1(side == TwoSides.ONE ? new AttributeModification<>(expectedState, OperationType.SET) : null)
                        .connected2(side == TwoSides.TWO ? new AttributeModification<>(expectedState, OperationType.SET) : null)
                        .build();
        String modificationInfosJson = mapper.writeValueAsString(modificationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> modifResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(modifResult.isPresent());

        if (!Objects.isNull(errorMessage)) {
            // change not applied
            assertThat(terminal.isConnected()).isNotEqualTo(expectedState);
            assertEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, modifResult.get().getApplicationStatus());
            assertLogMessage("BRANCH_MODIFICATION_ERROR : " + errorMessage, "MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR", reportService);
        } else {
            // connection state has changed as expected
            assertThat(terminal.isConnected()).isEqualTo(expectedState);
            assertEquals(NetworkModificationResult.ApplicationStatus.ALL_OK, modifResult.get().getApplicationStatus());

            // try to modify again => no change on connection state
            mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                    .andExpect(status().isOk());
            assertThat(terminal.isConnected()).isEqualTo(expectedState);
        }
    }
}

