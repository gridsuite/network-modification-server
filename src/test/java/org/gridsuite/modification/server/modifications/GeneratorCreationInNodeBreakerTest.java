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
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_GENERATOR_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class GeneratorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        // create new generator in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        return GeneratorCreationInfos.builder()
                .equipmentId("idGenerator1")
                .equipmentName("idGenerator1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .energySource(EnergySource.HYDRO)
                .minActivePower(100.0)
                .maxActivePower(600.0)
                .ratedNominalPower(10.)
                .activePowerSetpoint(400.)
                .reactivePowerSetpoint(50.)
                .voltageRegulationOn(true)
                .voltageSetpoint(225.)
                .stepUpTransformerReactance(60.0)
                .transientReactance(61.0)
                .minimumReactivePower(20.0)
                .maximumReactivePower(25.0)
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
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GeneratorCreationInfos.builder()
                .equipmentId("idGenerator2")
                .equipmentName("nameGeneratorModified")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .energySource(EnergySource.SOLAR)
                .minActivePower(101.0)
                .maxActivePower(601.0)
                .ratedNominalPower(11.)
                .activePowerSetpoint(401.)
                .reactivePowerSetpoint(51.)
                .voltageRegulationOn(true)
                .voltageSetpoint(226.)
                .stepUpTransformerReactance(61.0)
                .transientReactance(62.0)
                .minimumReactivePower(23.0)
                .maximumReactivePower(26.0)
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
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getGenerator("idGenerator1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator1")).count());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getGenerator("idGenerator1"));
        assertEquals(0, getNetwork().getVoltageLevel("v2").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator1")).count());
    }

    @Test
    public void testCreateWithErrors() throws Exception {
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
        generatorCreationInfos.setMinActivePower(Double.NaN);
        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Generator 'idGenerator1': invalid value (NaN) for minimum P",
                generatorCreationInfos.getErrorType().name(), reportService);

        // invalid min max reactive limit
        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMinimumReactivePower(Double.NaN);

        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1' : minimum reactive power is not set").getMessage(),
            generatorCreationInfos.getErrorType().name(), reportService);

        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMaximumReactivePower(Double.NaN);

        generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1' : maximum reactive power is not set").getMessage(),
            generatorCreationInfos.getErrorType().name(), reportService);

        generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setReactiveCapabilityCurve(false);
        generatorCreationInfos.setMinimumReactivePower(200.);
        generatorCreationInfos.setMaximumReactivePower(100.);

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
    public void testCreateWithShortCircuitErrors() throws Exception {
        // invalid short circuit transient reactance
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setTransientReactance(Double.NaN);

        String generatorCreationInfosJson = mapper.writeValueAsString(generatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("cannot add short-circuit extension on generator with id=idGenerator1 : Undefined directTransX", "ShortCircuitExtensionAddError", reportService);
    }
}
