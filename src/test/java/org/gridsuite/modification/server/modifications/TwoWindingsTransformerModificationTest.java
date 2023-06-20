/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.LoadingLimits;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Tag("IntegrationTest")
public class TwoWindingsTransformerModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return TwoWindingsTransformerModificationInfos.builder().equipmentId("trf1")
                .equipmentName(new AttributeModification<>("2wt modified name", OperationType.SET))
                .seriesResistance(new AttributeModification<>(1., OperationType.SET))
                .seriesReactance(new AttributeModification<>(2., OperationType.SET))
                .magnetizingConductance(new AttributeModification<>(3., OperationType.SET))
                .magnetizingSusceptance(new AttributeModification<>(4., OperationType.SET))
                .ratedVoltage1(new AttributeModification<>(5., OperationType.SET))
                .ratedVoltage2(new AttributeModification<>(6., OperationType.SET))
                .ratedS(new AttributeModification<>(7., OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(null)
                                .name("name31")
                                .value(null)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(32)
                                .name("name32")
                                .value(42.0)
                                .build()))
                        .build())
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return TwoWindingsTransformerModificationInfos.builder().equipmentId("trf1")
                .equipmentName(new AttributeModification<>("2wt modified name again", OperationType.SET))
                .seriesResistance(new AttributeModification<>(1.1, OperationType.SET))
                .seriesReactance(new AttributeModification<>(2.1, OperationType.SET))
                .magnetizingConductance(new AttributeModification<>(3.1, OperationType.SET))
                .magnetizingSusceptance(new AttributeModification<>(4.1, OperationType.SET))
                .ratedVoltage1(new AttributeModification<>(5.1, OperationType.SET))
                .ratedVoltage2(new AttributeModification<>(6.1, OperationType.SET))
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
                .build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
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
        assertEquals(Double.NaN, modifiedTwoWindingsTransformer.getNullableCurrentLimits1().getPermanentLimit());
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
    }

    @Override
    protected void assertNetworkAfterDeletion() {
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
}

