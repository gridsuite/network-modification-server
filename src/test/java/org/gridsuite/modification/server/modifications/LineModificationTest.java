/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.LoadingLimits.TemporaryLimit;

import lombok.SneakyThrows;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherLineModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */

public class LineModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineModificationInfos.builder().equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
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
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineModificationInfos.builder().equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified1", OperationType.SET))
                .seriesReactance(new AttributeModification<>(1.1, OperationType.SET))
                .seriesResistance(new AttributeModification<>(2.1, OperationType.SET))
                .shuntConductance1(new AttributeModification<>(11.1, OperationType.SET))
                .shuntSusceptance1(new AttributeModification<>(12.1, OperationType.SET))
                .shuntConductance2(new AttributeModification<>(13.1, OperationType.SET))
                .shuntSusceptance2(new AttributeModification<>(14.1, OperationType.SET))
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
    protected MatcherLineModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLineModificationInfos
                .createMatcherLineModificationInfos((LineModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        Line modifiedLine = getNetwork().getLine("line1");
        assertEquals("LineModified", modifiedLine.getNameOrId());
        assertEquals(1.0, modifiedLine.getR());
        assertEquals(1.0, modifiedLine.getX());
        assertEquals(1.0, modifiedLine.getG1());
        assertEquals(1.0, modifiedLine.getB1());
        assertEquals(2.0, modifiedLine.getG2());
        assertEquals(2.0, modifiedLine.getB2());
        assertEquals(Double.NaN, modifiedLine.getNullableCurrentLimits1().getPermanentLimit());
        TemporaryLimit temporaryLimit = modifiedLine.getNullableCurrentLimits1().getTemporaryLimit(Integer.MAX_VALUE);
        assertEquals(Integer.MAX_VALUE, temporaryLimit.getAcceptableDuration());
        assertEquals("name31", temporaryLimit.getName());
        assertEquals(Double.MAX_VALUE, temporaryLimit.getValue());
        assertEquals(22.0, modifiedLine.getNullableCurrentLimits2().getPermanentLimit());
        temporaryLimit = modifiedLine.getNullableCurrentLimits2().getTemporaryLimit(32);
        assertEquals(32, temporaryLimit.getAcceptableDuration());
        assertEquals("name32", temporaryLimit.getName());
        assertEquals(42.0, temporaryLimit.getValue());

    }

    @Override
    protected void assertNetworkAfterDeletion() {
        Line line = getNetwork().getLine("line1");
        assertEquals("line1", line.getNameOrId());
        assertEquals(1.0, line.getR());
        assertEquals(1.0, line.getX());
        assertEquals(1.0, line.getG1());
        assertEquals(1.0, line.getB1());
        assertEquals(2.0, line.getG2());
        assertEquals(2.0, line.getB2());
        assertNull(line.getNullableCurrentLimits1());
        assertNull(line.getNullableCurrentLimits2());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();
        lineModificationInfos.setEquipmentId("lineNotFound");
        String lineModificationInfosJson = mapper.writeValueAsString(lineModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri())
                .content(lineModificationInfosJson)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "Line lineNotFound does not exist in network").getMessage(),
                lineModificationInfos.getErrorType().name(), reportService);
    }

    @SneakyThrows
    @Test
    public void testPermanentLimitUnchanged() {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();

        lineModificationInfos.getCurrentLimits1().setPermanentLimit(null);
        lineModificationInfos.getCurrentLimits2().setPermanentLimit(null);
        String modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification, createMatcher(lineModificationInfos));
    }

    @SneakyThrows
    @Test
    public void testCharacteristicsModification() {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) buildModification();

        // Modify Series Reactance
        lineModificationInfos.setSeriesReactance(new AttributeModification<>(1.0, OperationType.SET));

        String modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify Series Resistance
        lineModificationInfos.setSeriesReactance(null);
        lineModificationInfos.setSeriesResistance(new AttributeModification<>(2.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify Shunt Conductance1
        lineModificationInfos.setSeriesResistance(null);
        lineModificationInfos.setShuntConductance1(new AttributeModification<>(11.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify Shunt Susceptance1
        lineModificationInfos.setShuntConductance1(null);
        lineModificationInfos.setShuntSusceptance1(new AttributeModification<>(12.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify Shunt Conductance2
        lineModificationInfos.setShuntSusceptance1(null);
        lineModificationInfos.setShuntConductance2(new AttributeModification<>(13.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify Shunt Susceptance2
        lineModificationInfos.setShuntConductance2(null);
        lineModificationInfos.setShuntSusceptance2(new AttributeModification<>(14.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(5);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // no modification
        lineModificationInfos.setShuntSusceptance2(null);
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(6);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify all
        lineModificationInfos.setSeriesReactance(new AttributeModification<>(1.0, OperationType.SET));
        lineModificationInfos.setSeriesResistance(new AttributeModification<>(2.0, OperationType.SET));
        lineModificationInfos.setShuntConductance1(new AttributeModification<>(11.0, OperationType.SET));
        lineModificationInfos.setShuntSusceptance1(new AttributeModification<>(12.0, OperationType.SET));
        lineModificationInfos.setShuntConductance2(new AttributeModification<>(13.0, OperationType.SET));
        lineModificationInfos.setShuntSusceptance2(new AttributeModification<>(14.0, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true)
                        .get(7);

        assertThat(createdModification, createMatcher(lineModificationInfos));
    }

    @SneakyThrows
    @Test
    public void testTemporaryLimitsModification() {
        Line line = getNetwork().getLine("line1");
        line.newCurrentLimits1()
                .setPermanentLimit(10.0)
                .beginTemporaryLimit()
                .setName("name31")
                .setAcceptableDuration(Integer.MAX_VALUE)
                .setValue(Double.MAX_VALUE)
                .endTemporaryLimit()
                .add();
        line.newCurrentLimits2()
                .setPermanentLimit(11.0)
                .beginTemporaryLimit()
                .setName("name32")
                .setAcceptableDuration(32)
                .setValue(15.0)
                .endTemporaryLimit()
                .beginTemporaryLimit()
                .setName("name33")
                .setAcceptableDuration(33)
                .setValue(15.0)
                .endTemporaryLimit()
                .add();
        LineModificationInfos lineModificationInfos = LineModificationInfos.builder().equipmentId("line1")
                .equipmentName(new AttributeModification<>("LineModified", OperationType.SET))
                .currentLimits1(CurrentLimitsModificationInfos.builder()
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(null)
                                .name("name31")
                                .value(22.0)
                                .modificationType(TemporaryLimitModificationType.MODIFIED)
                                .build()))
                        .build())
                .currentLimits2(CurrentLimitsModificationInfos.builder()
                        .permanentLimit(22.0)
                        .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                                .acceptableDuration(33)
                                .name("name33")
                                .value(15.0)
                                .modificationType(TemporaryLimitModificationType.DELETED)
                                .build()))
                        .build())
                .build();
        String modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineModificationInfos createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification, createMatcher(lineModificationInfos));

        // Modify name and no modification on temporary limits
        line.setName(null);
        LineModificationInfos lineModificationInfos1 = LineModificationInfos.builder()
                .equipmentId("line1")
                .equipmentName(new AttributeModification<>("ModifiedName", OperationType.SET))
                .build();
        modificationToCreateJson = mapper.writeValueAsString(lineModificationInfos1);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (LineModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification, createMatcher(lineModificationInfos1));
    }
}
