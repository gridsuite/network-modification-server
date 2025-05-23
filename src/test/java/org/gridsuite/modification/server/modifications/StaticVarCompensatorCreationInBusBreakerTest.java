/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.StaticVarCompensatorCreationInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

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

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
class StaticVarCompensatorCreationInBusBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createBusBreaker(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return StaticVarCompensatorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idStaticVarCompensator2")
                .equipmentName("nameStaticVarCompensator2")
                .voltageLevelId("v1")
                .busOrBusbarSectionId("bus1")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .maxSusceptance(null)
                .minSusceptance(null)
                .maxQAtNominalV(224.0)
                .minQAtNominalV(200.0)
                .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
                .voltageSetpoint(120.0)
                .reactivePowerSetpoint(300.0)
                .voltageRegulationType(VoltageRegulationType.DISTANT)
                .regulatingTerminalVlId("v1")
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .standbyAutomatonOn(true)
                .standby(true)
                .b0(221.0)
                .lowVoltageSetpoint(200.0)
                .highVoltageSetpoint(400.0)
                .lowVoltageThreshold(250.0)
                .highVoltageThreshold(300.0)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return StaticVarCompensatorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idStaticVarCompensator2Edited")
                .equipmentName("staticVarCompensatorNameEdited")
                .voltageRegulationType(VoltageRegulationType.LOCAL)
                .regulatingTerminalId("idStaticVarCompensator1")
                .regulatingTerminalType("STATIC_VAR_COMPENSATOR")
                .regulatingTerminalVlId("v1")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator2"));
        assertEquals(1, getNetwork().getVoltageLevel("v1").getStaticVarCompensatorStream()
                .filter(transformer -> transformer.getId().equals("idStaticVarCompensator2")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getStaticVarCompensator("idStaticVarCompensator2").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator2"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getStaticVarCompensatorStream()
                .filter(transformer -> transformer.getId().equals("idStaticVarCompensator2")).count());
    }

    @Test
    void testCreateWithBusBarSectionErrors() throws Exception {
        StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        staticVarCompensatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        String body = getJsonBody(staticVarCompensatorCreationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(body).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithRegulatedTerminalError() throws Exception {
        StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        staticVarCompensatorCreationInfos.setVoltageRegulationType(VoltageRegulationType.DISTANT);
        staticVarCompensatorCreationInfos.setRegulatingTerminalVlId("v1");
        staticVarCompensatorCreationInfos.setRegulatingTerminalId("test");
        staticVarCompensatorCreationInfos.setRegulatingTerminalType("STATIC_VAR_COMPENSATOR");

        String staticVarCompensatorInfosJson = getJsonBody(staticVarCompensatorCreationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(staticVarCompensatorInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=test not found with type STATIC_VAR_COMPENSATOR").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idStaticVarCompensator2", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idStaticVarCompensator2Edited", createdValues.get("equipmentId"));
    }
}
