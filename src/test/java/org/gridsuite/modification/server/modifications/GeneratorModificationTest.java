/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.utils.MatcherGeneratorModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class GeneratorModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GeneratorModificationInfos.builder()
                .type(ModificationType.GENERATOR_MODIFICATION)
                .equipmentId("idGenerator")
                .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(48.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
                .minActivePower(new AttributeModification<>(0., OperationType.SET))
                .maxActivePower(new AttributeModification<>(100., OperationType.SET))
                .ratedNominalPower(new AttributeModification<>(220., OperationType.SET))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GeneratorModificationInfos.builder()
                .type(ModificationType.GENERATOR_MODIFICATION)
                .equipmentId("idGenerator")
                .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1GeneratorEdited", OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(81.0, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(41.0, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(49.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
                .minActivePower(new AttributeModification<>(1., OperationType.SET))
                .maxActivePower(new AttributeModification<>(102., OperationType.SET))
                .ratedNominalPower(new AttributeModification<>(221., OperationType.SET))
                .build();
    }

    @Override
    protected MatcherGeneratorModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGeneratorModificationInfos.createMatcherGeneratorModificationInfos((GeneratorModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        Generator modifiedGenerator = getNetwork().getGenerator("idGenerator");
        assertEquals("newV1Generator", modifiedGenerator.getNameOrId());
        assertEquals(EnergySource.SOLAR, modifiedGenerator.getEnergySource());
        assertEquals(80.0, modifiedGenerator.getTargetP());
        assertEquals(40.0, modifiedGenerator.getTargetQ());
        assertEquals(48.0, modifiedGenerator.getTargetV());
        assertEquals(true, modifiedGenerator.isVoltageRegulatorOn());
        assertEquals(0., modifiedGenerator.getMinP());
        assertEquals(100., modifiedGenerator.getMaxP());
        assertEquals(220., modifiedGenerator.getRatedS());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        Generator generator = getNetwork().getGenerator("idGenerator");
        assertEquals("idGenerator", generator.getNameOrId());
        assertEquals(EnergySource.OTHER, generator.getEnergySource());
        assertEquals(42.1, generator.getTargetP());
        assertEquals(1.0, generator.getTargetQ());
        assertEquals(Double.NaN, generator.getTargetV());
        assertEquals(false, generator.isVoltageRegulatorOn());
        assertEquals(-1.1, generator.getMinP());
        assertEquals(1000.0, generator.getMaxP());
        assertEquals(Double.NaN, generator.getRatedS());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        // Unset an attribute that should not be null
        generatorModificationInfos.setEnergySource(new AttributeModification<>(null, OperationType.UNSET));

        String generatorModificationInfosJson = mapper.writeValueAsString(generatorModificationInfos);
        MvcResult mvcResult = mockMvc
                .perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Generator '" + "idGenerator" + "': energy source is not set").getMessage());

    }
}
