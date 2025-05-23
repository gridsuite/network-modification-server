/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.GeneratorCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class GeneratorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GeneratorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator2")
                .equipmentName("nameGenerator2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .energySource(EnergySource.HYDRO)
                .minP(100.0)
                .maxP(600.0)
                .ratedS(10.)
                .targetP(400.)
                .targetQ(50.)
                .voltageRegulationOn(true)
                .targetV(225.)
                .stepUpTransformerX(60.0)
                .directTransX(61.0)
                .minQ(20.0)
                .maxQ(25.0)
                .plannedActivePowerSetPoint(222.)
                .marginalCost(0.50)
                .plannedOutageRate(.85)
                .forcedOutageRate(.96)
                .droop(5f)
                .participate(true)
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .regulatingTerminalVlId("v1")
                .qPercent(25.)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurvePointsInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GeneratorCreationInfos.builder()
                .equipmentId("idGenerator2Edited")
                .stashed(false)
                .equipmentName("nameGeneratorModified")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .energySource(EnergySource.SOLAR)
                .minP(101.0)
                .maxP(601.0)
                .ratedS(11.)
                .targetP(401.)
                .targetQ(51.)
                .voltageRegulationOn(true)
                .targetV(226.)
                .stepUpTransformerX(61.0)
                .directTransX(62.0)
                .minQ(23.0)
                .maxQ(26.0)
                .plannedActivePowerSetPoint(111.)
                .marginalCost(0.40)
                .plannedOutageRate(.45)
                .forcedOutageRate(.66)
                .droop(6f)
                .participate(true)
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .regulatingTerminalVlId("v1")
                .qPercent(29.)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(1.0, 2.0, 2.1),
                        new ReactiveCapabilityCurvePointsInfos(6.6, 8.8, 11.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getGenerator("idGenerator2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator2")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getGenerator("idGenerator2").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getGenerator("idGenerator2"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator2")).count());
    }

    @Test
    void testCreateWithBusbarSectionErrors() throws Exception {
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        String body = getJsonBody(generatorCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(body).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithRegulatedTerminalError() throws Exception {
         // invalid regulating terminal id <---> regulation terminal type
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setRegulatingTerminalType("LINE");
        generatorCreationInfos.setRegulatingTerminalId("titi");

        String generatorCreationInfosJson = getJsonBody(generatorCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=titi not found with type LINE").getMessage(),
            ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator2", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator2Edited", createdValues.get("equipmentId"));
    }
}
