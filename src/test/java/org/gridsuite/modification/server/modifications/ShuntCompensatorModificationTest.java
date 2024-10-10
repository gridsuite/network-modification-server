/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_SHUNT_COMPENSATOR_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.SHUNT_COMPENSATOR_NOT_FOUND;
import static org.gridsuite.modification.server.utils.NetworkUtil.createShuntCompensator;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
class ShuntCompensatorModificationTest extends AbstractInjectionModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Test
    void testEquipmentWithWrongId() throws Exception {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("wrong id")
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SHUNT_COMPENSATOR_NOT_FOUND,
                        "Shunt compensator wrong id does not exist in network").getMessage(),
                shuntCompensator.getErrorType().name(), reportService);
    }

    @Test
    void testWrongMaximumSectionCount() throws Exception {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .sectionCount(new AttributeModification<>(3, OperationType.SET))
                .maximumSectionCount(new AttributeModification<>(-1, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR,
                        String.format("Maximum section count should be greater or equal to 1")).getMessage(),
                shuntCompensator.getErrorType().name(), reportService);
    }

    @Test
    void testWrongSectionCount() throws Exception {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .sectionCount(new AttributeModification<>(3, OperationType.SET))
                .maximumSectionCount(new AttributeModification<>(1, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR,
                        String.format("Section count should be between 0 and Maximum section count (1), actual : 3")).getMessage(),
                shuntCompensator.getErrorType().name(), reportService);
    }

    @Test
    void testWrongSectionCountChangeSectionCount() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        var shuntCompensatorModifications = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v7shunt")
                .sectionCount(new AttributeModification<>(3, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensatorModifications)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR,
                        String.format("Section count should be between 0 and Maximum section count (1), actual : 3")).getMessage(),
                shuntCompensatorModifications.getErrorType().name(), reportService);
    }

    @Test
    void testWrongSectionCountChangeMaximumSectionCount() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        var shuntCompensatorModifications = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v7shunt")
                .sectionCount(new AttributeModification<>(-1, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensatorModifications)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR,
                         "Section count should be between 0 and Maximum section count (1), actual : -1").getMessage(),
                shuntCompensatorModifications.getErrorType().name(), reportService);
    }

    @Test
    void testNegativeQmaxAtNominalV() throws Exception {
        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v5shunt")
                .maxQAtNominalV(new AttributeModification<>(-15.0, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(MODIFY_SHUNT_COMPENSATOR_ERROR,
                        "Qmax at nominal voltage should be greater or equal to 0").getMessage(),
                shuntCompensator.getErrorType().name(), reportService);
    }

    @Test
    void testCreateModificationWithShuntCompensatorType() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        assertEquals(1.0, model.getBPerSection(), 0);
        ShuntCompensatorModificationInfos modificationInfos = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v7shunt")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.REACTOR, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(-1.0, model.getBPerSection(), 0);
    }

    @Test
    void testCreateModificationWithSusceptancePerSection() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        assertEquals(1.0, model.getBPerSection(), 0);
        ShuntCompensatorModificationInfos modificationInfos = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v7shunt")
                .maxSusceptance(AttributeModification.toAttributeModification(3.0, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(3.0, model.getBPerSection(), 0);
    }

    @Test
    void testCreateModificationWithSections() throws Exception {
        var shuntCompensatorToModify = getNetwork().getShuntCompensator("v5shunt");
        var model = shuntCompensatorToModify.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);

        var shuntCompensator = ShuntCompensatorModificationInfos.builder()
                .equipmentId("v5shunt")
                .maximumSectionCount(AttributeModification.toAttributeModification(3, OperationType.SET))
                .sectionCount(AttributeModification.toAttributeModification(2, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(shuntCompensator)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(3, shuntCompensatorToModify.getMaximumSectionCount());
        assertEquals(2, shuntCompensatorToModify.getSectionCount());
    }

    @Test
    void testCreateModificationWithQAtNominalV() throws Exception {
        VoltageLevel v5 = getNetwork().getVoltageLevel("v5");
        createShuntCompensator(v5, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);
        VoltageLevel v6 = getNetwork().getVoltageLevel("v6");
        createShuntCompensator(v6, "v8shunt", "v8shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v8shunt", 50, ConnectablePosition.Direction.BOTTOM);

        ShuntCompensatorModificationInfos modificationInfos1 = ShuntCompensatorModificationInfos.builder()
                        .stashed(false)
                        .equipmentId("v7shunt")
                        .maxQAtNominalV(new AttributeModification<>(30.5, OperationType.SET))
                        .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.REACTOR, OperationType.SET))
                        .build();

        ShuntCompensatorModificationInfos modificationInfos2 = ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v8shunt")
                .maxQAtNominalV(new AttributeModification<>(30.5, OperationType.SET))
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos1)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationInfos2)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        var shuntCompensator1 = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator1.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(-2.1121E-4, model.getBPerSection(), 0.0001);

        var shuntCompensator2 = getNetwork().getShuntCompensator("v8shunt");
        var model2 = shuntCompensator2.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model2);
        assertEquals(2.1121E-4, model2.getBPerSection(), 0.0001);
    }

    @Override
    protected ModificationInfos buildModification() {
        VoltageLevel v2 = getNetwork().getVoltageLevel("v2");
        createShuntCompensator(v2, "v7shunt", "v7shunt", 25, 225., 10, true, 1, 1, 2, 1, "feeder_v7shunt", 40, ConnectablePosition.Direction.BOTTOM);

        return ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v7shunt")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .maxQAtNominalV(new AttributeModification<>(15.0, OperationType.SET))
                .maximumSectionCount(new AttributeModification<>(1, OperationType.SET))
                .sectionCount(new AttributeModification<>(1, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();

    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("v2shunt")
                .maxSusceptance(new AttributeModification<>(0.5, OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        var shuntCompensator = getNetwork().getShuntCompensator("v7shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(2.9629E-4, model.getBPerSection(), 0.0001);
        assertEquals(PROPERTY_VALUE, getNetwork().getShuntCompensator("v7shunt").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        var shuntCompensator = getNetwork().getShuntCompensator("v2shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(1.0, model.getBPerSection(), 0);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v7shunt", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("SHUNT_COMPENSATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v2shunt", updatedValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        ShuntCompensatorModificationInfos shuntModificationInfos =
                ShuntCompensatorModificationInfos.builder()
                        .stashed(false)
                        .equipmentId("v2shunt")
                        .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                        .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                        .build();
        assertChangeConnectionState(getNetwork().getShuntCompensator("v2shunt"), shuntModificationInfos, false);
    }

    @Test
    void testConnection() throws Exception {
        ShuntCompensatorModificationInfos shuntModificationInfos =
                ShuntCompensatorModificationInfos.builder()
                        .stashed(false)
                        .equipmentId("v2shunt")
                        .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                        .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                        .build();
        assertChangeConnectionState(getNetwork().getShuntCompensator("v2shunt"), shuntModificationInfos, true);
    }
}
