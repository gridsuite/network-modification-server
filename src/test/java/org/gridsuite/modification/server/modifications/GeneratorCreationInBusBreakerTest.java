/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;

import lombok.SneakyThrows;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.utils.MatcherGeneratorCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class GeneratorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GeneratorCreationInfos.builder()
                .type(ModificationType.GENERATOR_CREATION)
                .equipmentId("idGenerator2")
                .equipmentName("nameGenerator2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
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
                .droop(5f)
                .participate(true)
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
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
                .type(ModificationType.GENERATOR_CREATION)
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
    protected MatcherGeneratorCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos((GeneratorCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getGenerator("idGenerator2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator2")).count());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getGenerator("idGenerator2"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getGeneratorStream()
                .filter(transformer -> transformer.getId().equals("idGenerator2")).count());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        GeneratorCreationInfos generatorCreationInfos = (GeneratorCreationInfos) buildModification();
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(generatorCreationInfos)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().is4xxClientError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());
    }
}
