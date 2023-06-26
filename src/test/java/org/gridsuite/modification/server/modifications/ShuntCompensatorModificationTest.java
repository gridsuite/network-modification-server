/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.MatcherShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.SHUNT_COMPENSATOR_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class ShuntCompensatorModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @SneakyThrows
    @Test
    public void testEquipmentWithWrongId() {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .equipmentId("wrong id")
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SHUNT_COMPENSATOR_NOT_FOUND,
                        String.format("Shunt compensator wrong id does not exist in network")).getMessage(),
                shuntCompensator.getErrorType().name(), reportService);
    }

    @SneakyThrows
    @Test
    public void testWrongVoltageLevelId() {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .voltageLevelId("wrongVLId")
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND,
                        String.format("Voltage level wrongVLId does not exist in network")).getMessage(),
                shuntCompensator.getErrorType().name(), reportService);
    }

    @SneakyThrows
    @Test
    public void testCreateModificationWithShuntCompensatorType() {
        var shuntCompensator = getNetwork().getShuntCompensator("v5shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        assertEquals(1.0, model.getBPerSection(), 0);
        ShuntCompensatorModificationInfos modificationInfos = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .voltageLevelId("v5")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.REACTOR, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(-1.0, model.getBPerSection(), 0);
    }

    @SneakyThrows
    @Test
    public void testCreateModificationWithSusceptancePerSection() {
        var shuntCompensator = getNetwork().getShuntCompensator("v5shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        assertEquals(1.0, model.getBPerSection(), 0);
        ShuntCompensatorModificationInfos modificationInfos = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .voltageLevelId("v5")
                .susceptancePerSection(AttributeModification.toAttributeModification(3.0, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(3.0, model.getBPerSection(), 0);
    }

    @SneakyThrows
    @Test
    public void testCreateModificationWithQAtNominalV() {
        ShuntCompensatorModificationInfos modificationInfos1 = ShuntCompensatorModificationInfos.builder()
                        .equipmentId("v5shunt")
                        .voltageLevelId("v5")
                        .qAtNominalV(new AttributeModification<>(30.5, OperationType.SET))
                        .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.REACTOR, OperationType.SET))
                        .build();

        ShuntCompensatorModificationInfos modificationInfos2 = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v6shunt")
                .voltageLevelId("v6")
                .qAtNominalV(new AttributeModification<>(30.5, OperationType.SET))
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos1)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos2)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var shuntCompensator1 = getNetwork().getShuntCompensator("v5shunt");
        var model = shuntCompensator1.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(-2.1121E-4, model.getBPerSection(), 0.0001);

        var shuntCompensator2 = getNetwork().getShuntCompensator("v6shunt");
        var model2 = shuntCompensator2.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model2);
        assertEquals(2.1121E-4, model2.getBPerSection(), 0.0001);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorModificationInfos.builder()
                .equipmentId("v2shunt")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .qAtNominalV(new AttributeModification<>(15.0, OperationType.SET))
                .voltageLevelId("v2")
                .build();

    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorModificationInfos.builder()
                .equipmentId("v2shunt")
                .voltageLevelId("v2")
                .susceptancePerSection(new AttributeModification<>(0.5, OperationType.SET))
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherShuntCompensatorModificationInfos.createMatcherShuntCompensatorModificationInfos((ShuntCompensatorModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        var shuntCompensator = getNetwork().getShuntCompensator("v2shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(2.9629E-4, model.getBPerSection(), 0.0001);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        var shuntCompensator = getNetwork().getShuntCompensator("v2shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(1.0, model.getBPerSection(), 0);
    }
}
