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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class GeneratorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new generator in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return GeneratorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator1")
                .equipmentName("idGenerator1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
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
                .plannedActivePowerSetPoint(111.)
                .marginalCost(0.40)
                .plannedOutageRate(.45)
                .forcedOutageRate(.66)
                .droop(5f)
                .participate(true)
                .regulatingTerminalId("v2load")
                .regulatingTerminalType("LOAD")
                .regulatingTerminalVlId("v1")
                .qPercent(25.)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurveCreationInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GeneratorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator2")
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
                .plannedActivePowerSetPoint(222.)
                .marginalCost(0.50)
                .plannedOutageRate(.85)
                .forcedOutageRate(.96)
                .droop(6f)
                .participate(true)
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .regulatingTerminalVlId("v1")
                .qPercent(29.)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(1.0, 2.0, 2.1),
                        new ReactiveCapabilityCurveCreationInfos(6.6, 8.8, 11.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getGenerator("idGenerator1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getGenerator("idGenerator1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getGenerator("idGenerator1"));
        assertEquals(0, getNetwork().getVoltageLevel("v2").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator1")).count());
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // invalid Generator id
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setEquipmentId("");
        String generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", generatorCreationInfos.getErrorType().name(), reportService);

        // not found voltage level
        generatorCreationInfos.setEquipmentId("idGenerator1");
        generatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                generatorCreationInfos.getErrorType().name(), reportService);

        // not found busbar section
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage(),
                generatorCreationInfos.getErrorType().name(), reportService);

        // invalid min active power
        generatorCreationInfos.setVoltageLevelId("v2");

        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfos.setMinP(Double.NaN);
        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Generator 'idGenerator1': invalid value (NaN) for minimum P",
                generatorCreationInfos.getErrorType().name(), reportService);

        // invalid min max reactive limit
        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMinQ(Double.NaN);

        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1' : minimum reactive power is not set").getMessage(),
            generatorCreationInfos.getErrorType().name(), reportService);

        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMaxQ(Double.NaN);

        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1' : maximum reactive power is not set").getMessage(),
            generatorCreationInfos.getErrorType().name(), reportService);

        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMinQ(200.);
        generatorCreationInfos.setMaxQ(100.);

        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1' : maximum reactive power is expected to be greater than or equal to minimum reactive power").getMessage(),
            generatorCreationInfos.getErrorType().name(), reportService);

        // invalid reactive capability curve limit
        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.getReactiveCapabilityCurvePoints().get(0).setP(Double.NaN);

        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1' : P is not set in a reactive capability curve limits point").getMessage(),
            generatorCreationInfos.getErrorType().name(), reportService);

        // try to create an existing generator
        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setEquipmentId("v5generator");
        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(GENERATOR_ALREADY_EXISTS, "v5generator").getMessage(),
                generatorCreationInfos.getErrorType().name(), reportService);

        // Test create generator on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the generator cannot be created
        generatorCreationInfos.setEquipmentId("idGenerator3");
        generatorCreationInfos.setEquipmentName("nameGenerator3");
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationResult.isEmpty());  // no modifications returned
        assertNull(getNetwork().getGenerator("idGenerator3"));  // generator was not created
        testNetworkModificationsCount(getGroupId(), 10);  // new modification stored in the database
    }

    @Test
    void testCreateWithShortCircuitErrors() throws Exception {
        // invalid short circuit transient reactance
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setDirectTransX(Double.NaN);

        String generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("cannot add short-circuit extension on generator with id=idGenerator1 : Undefined directTransX", "ShortCircuitExtensionAddError", reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator1", updatedValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator2", updatedValues.get("equipmentId"));
    }
}
