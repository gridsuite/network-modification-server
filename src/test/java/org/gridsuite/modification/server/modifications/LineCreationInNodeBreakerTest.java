/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLineCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_LINE_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherLineCreationInfos.createMatcherLineCreationInfos;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LineCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    @Test
    @SneakyThrows
    public void testCreateWithBadVariant() {
        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        LineCreationInfos modificationToCreate = (LineCreationInfos) buildModification();
        modificationToCreate.setEquipmentId("idLine2");
        modificationToCreate.setEquipmentName("nameLine2");
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertNotNull(modifications);
        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(getNetwork().getLine("idLine2"));  // line was not created
        testNetworkModificationsCount(getGroupId(), 1);  // new modification stored in the database
    }

    @Test
    @SneakyThrows
    public void testCreateWithErrors() {
        LineCreationInfos lineCreationInfos = (LineCreationInfos) buildModification();
        lineCreationInfos.setEquipmentId("");
        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is5xxServerError(),
                content().string(new NetworkModificationException(CREATE_LINE_ERROR, "Invalid id ''").getMessage())
            );

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is4xxClientError(),
                content().string(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage())
            );

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is5xxServerError(),
                content().string(new NetworkModificationException(CREATE_LINE_ERROR, "Identifiable notFoundBusbarSection1 not found.").getMessage())
            );

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setSeriesResistance(Double.NaN);
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is5xxServerError(),
                content().string(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': r is invalid").getMessage())
            );

        lineCreationInfos.setSeriesResistance(100.0);
        lineCreationInfos.setSeriesReactance(Double.NaN);
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is5xxServerError(),
                content().string(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': x is invalid").getMessage())
            );
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineCreationInfos.builder()
                .equipmentId("idLine")
                .equipmentName("nameLine")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connectionName1("cn1Line")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2Line")
                .connectionDirection2(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineCreationInfos.builder()
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .seriesResistance(110.0)
                .seriesReactance(110.0)
                .shuntConductance1(15.0)
                .shuntSusceptance1(15.0)
                .shuntConductance2(25.0)
                .shuntSusceptance2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.).build())
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .connectionPosition1(0)
                .connectionPosition2(0)
                .build();
    }

    @Override
    protected MatcherLineCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherLineCreationInfos((LineCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLine("idLine"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLine("idLine"));
    }
}
