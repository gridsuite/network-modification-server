/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Collections;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.assertions.Assertions.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class LineCreationInBusBreakerTest extends AbstractNetworkModificationTest {

    @Test
    public void testCreateWithErrors() throws Exception {
        LineCreationInfos lineCreationInfos = (LineCreationInfos) buildModification();
        lineCreationInfos.setBusOrBusbarSectionId2("notFoundBus");
        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                lineCreationInfos.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateLineOptionalParameters() throws Exception {
        // create new line without shunt conductance or reactance
        LineCreationInfos lineCreationInfosNoShunt = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .build();

        String lineCreationInfosNoShuntJson = mapper.writeValueAsString(lineCreationInfosNoShunt);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosNoShuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(lineCreationInfosNoShunt);

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    public void testCreateLineOptionalParameters2() throws Exception {
        // create new line without shunt conductance or reactance
        LineCreationInfos lineCreationInfosNoShunt = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .build();

        lineCreationInfosNoShunt.setShuntConductance1(50.0);
        lineCreationInfosNoShunt.setShuntConductance2(null);
        lineCreationInfosNoShunt.setShuntSusceptance1(null);
        lineCreationInfosNoShunt.setShuntSusceptance2(60.0);

        String lineCreationInfosNoShuntJson = mapper.writeValueAsString(lineCreationInfosNoShunt);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosNoShuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(lineCreationInfosNoShunt);

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    public void testCreateLineOptionalParameters3() throws Exception {
        LineCreationInfos lineCreationInfosPermanentLimitOK = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(1.0).temporaryLimits(Collections.emptyList()).build())
                .build();

        String lineCreationInfosPermanentLimitOKJson = mapper.writeValueAsString(lineCreationInfosPermanentLimitOK);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosPermanentLimitOKJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(lineCreationInfosPermanentLimitOK);

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    public void testCreateLineOptionalParameters4() throws Exception {
        LineCreationInfos lineCreationInfosPermanentLimitOK = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build())
                .currentLimits2(null)
                .build();

        String lineCreationInfosPermanentLimitOKJson = mapper.writeValueAsString(lineCreationInfosPermanentLimitOK);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosPermanentLimitOKJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        lineCreationInfosPermanentLimitOK.setCurrentLimits2(null); // if permanentLimit is null then no currentLimit created
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(lineCreationInfosPermanentLimitOK);

        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    public void testCreateLineOptionalParameters5() throws Exception {
        LineCreationInfos lineCreationInfosPermanentLimitNOK = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(0.0).build())
                .build();
        String lineCreationInfosPermanentLimitNOKJson = mapper.writeValueAsString(lineCreationInfosPermanentLimitNOK);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosPermanentLimitNOKJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("AC Line 'idLine2': permanent limit must be defined and be > 0", lineCreationInfosPermanentLimitNOK.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateLineOptionalParameters6() throws Exception {
        LineCreationInfos lineCreationInfosOK = LineCreationInfos.builder()
                .stashed(false)
                .equipmentId("idLine3")
                .equipmentName("nameLine3")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(1.0).build())
                .build();

        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfosOK);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
        return LineCreationInfos.builder()
            .stashed(false)
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .seriesResistance(100.0)
            .seriesReactance(100.0)
            .shuntConductance1(10.0)
            .shuntSusceptance1(10.0)
            .shuntConductance2(20.0)
            .shuntSusceptance2(20.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build())
            .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.).temporaryLimits(Collections.emptyList()).build())
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
            .stashed(false)
            .equipmentId("idLineEdited1")
            .equipmentName("nameLineEdited1")
            .seriesResistance(200.0)
            .seriesReactance(200.0)
            .shuntConductance1(20.0)
            .shuntSusceptance1(20.0)
            .shuntConductance2(30.0)
            .shuntSusceptance2(30.0)
            .voltageLevelId1("v2")
            .busOrBusbarSectionId1("bus3")
            .voltageLevelId2("v3")
            .busOrBusbarSectionId2("bus4")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLine("idLine1"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLine("idLine1"));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLine1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLineEdited1", updatedValues.get("equipmentId"));
    }
}
