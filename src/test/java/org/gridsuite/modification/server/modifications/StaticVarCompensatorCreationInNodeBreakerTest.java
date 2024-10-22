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
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static com.powsybl.iidm.network.StaticVarCompensator.RegulationMode.OFF;
import static com.powsybl.iidm.network.StaticVarCompensator.RegulationMode.VOLTAGE;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
public class StaticVarCompensatorCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return StaticVarCompensatorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idStaticVarCompensator1")
                .equipmentName("nameStaticVarCompensator1")
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .maxSusceptance(224.0)
                .minSusceptance(200.0)
                .maxQAtNominalV(null)
                .minQAtNominalV(null)
                .regulationMode(StaticVarCompensator.RegulationMode.VOLTAGE)
                .voltageSetpoint(120.0)
                .reactivePowerSetpoint(300.0)
                .voltageRegulationType(VoltageRegulationType.LOCAL)
                .standbyAutomatonOn(false)
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return StaticVarCompensatorCreationInfos.builder()
                .stashed(false)
                .equipmentId("idStaticVarCompensator1Edited")
                .equipmentName("staticVarCompensatorNameEdited")
                .maxSusceptance(null)
                .minSusceptance(null)
                .maxQAtNominalV(224.0)
                .minQAtNominalV(200.0)
                .standbyAutomatonOn(true)
                .standby(true)
                .b0(null)
                .q0(221.0)
                .lowVoltageSetpoint(200.0)
                .highVoltageSetpoint(400.0)
                .lowVoltageThreshold(250.0)
                .highVoltageThreshold(300.0)
                .regulatingTerminalId("idGenerator1")
                .regulatingTerminalType("GENERATOR")
                .regulatingTerminalVlId("v1")
                .voltageRegulationType(VoltageRegulationType.DISTANT)
                .regulatingTerminalId("idStaticVarCompensator1")
                .regulatingTerminalType("STATIC_VAR_COMPENSATOR")
                .regulatingTerminalVlId("v2")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator1"));
        assertEquals(1, getNetwork().getVoltageLevel("v2").getStaticVarCompensatorStream()
                .filter(transformer -> transformer.getId().equals("idStaticVarCompensator1")).count());
        assertEquals(PROPERTY_VALUE, getNetwork().getStaticVarCompensator("idStaticVarCompensator1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator2"));
        assertEquals(0, getNetwork().getVoltageLevel("v2").getStaticVarCompensatorStream()
                .filter(transformer -> transformer.getId().equals("idStaticVarCompensator2")).count());
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        // invalid Generator id
        StaticVarCompensatorCreationInfos compensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        compensatorCreationInfos.setEquipmentId("");
        String compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", compensatorCreationInfos.getErrorType().name(), reportService);

        // try to create an existing cspr
        compensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        compensatorCreationInfos.setEquipmentId("v5Compensator");
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(STATIC_VAR_COMPENSATOR_ALREADY_EXISTS, "v5Compensator").getMessage(),
                compensatorCreationInfos.getErrorType().name(), reportService);

        // not found voltage level
        compensatorCreationInfos.setEquipmentId("idStaticVarCompensator2");
        compensatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                compensatorCreationInfos.getErrorType().name(), reportService);

        // not found busbar section
        compensatorCreationInfos.setVoltageLevelId("v2");
        compensatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage(),
                compensatorCreationInfos.getErrorType().name(), reportService);

        // invalid min susceptance
        compensatorCreationInfos.setVoltageLevelId("v2");
        compensatorCreationInfos.setBusOrBusbarSectionId("1B");
        compensatorCreationInfos.setMinSusceptance(null);
        compensatorCreationInfos.setMinQAtNominalV(null);

        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : StaticVarCompensator 'idStaticVarCompensator2' : minimum susceptance is not set",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setMinSusceptance(200.0);
        compensatorCreationInfos.setMaxSusceptance(null);
        compensatorCreationInfos.setMaxQAtNominalV(null);
        compensatorCreationInfos.setMinQAtNominalV(null);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : maximum susceptance is not set",
                compensatorCreationInfos.getErrorType().name(), reportService);

        compensatorCreationInfos.setMaxSusceptance(100.0);
        compensatorCreationInfos.setMinSusceptance(200.0);
        compensatorCreationInfos.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationInfos.setReactivePowerSetpoint(null);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : maximum susceptance is expected to be greater than or equal to minimum susceptance",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setMaxSusceptance(null);
        compensatorCreationInfos.setMinSusceptance(null);
        compensatorCreationInfos.setMaxQAtNominalV(200.0);
        compensatorCreationInfos.setMinQAtNominalV(300.0);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : maximum Q at nominal voltage is expected to be greater than or equal to minimum Q",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setMaxQAtNominalV(200.0);
        compensatorCreationInfos.setMinQAtNominalV(100.0);
        compensatorCreationInfos.setRegulationMode(StaticVarCompensator.RegulationMode.REACTIVE_POWER);
        compensatorCreationInfos.setReactivePowerSetpoint(null);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : Reactive power setpoint is not set",
                compensatorCreationInfos.getErrorType().name(), reportService);

        compensatorCreationInfos.setRegulationMode(StaticVarCompensator.RegulationMode.VOLTAGE);
        compensatorCreationInfos.setVoltageSetpoint(null);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator2' : Voltage setpoint is not set",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setEquipmentId("idStaticVarCompensator3");
        compensatorCreationInfos.setEquipmentName("nameStaticVarCompensator3");
        compensatorCreationInfos.setVoltageLevelId("v2");
        compensatorCreationInfos.setBusOrBusbarSectionId("1B");
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isEmpty());
        assertNull(getNetwork().getStaticVarCompensator("idStaticVarCompensator3"));
        testNetworkModificationsCount(getGroupId(), 11);
    }

    @Test
    public void testCreateWithStandbyAutomatonErrors() throws Exception {
        StaticVarCompensatorCreationInfos compensatorCreationInfos = (StaticVarCompensatorCreationInfos) buildModification();
        compensatorCreationInfos.setMaxSusceptance(null);
        compensatorCreationInfos.setMinSusceptance(null);
        compensatorCreationInfos.setMinQAtNominalV(200.0);
        compensatorCreationInfos.setMaxQAtNominalV(300.0);
        compensatorCreationInfos.setStandbyAutomatonOn(true);
        compensatorCreationInfos.setLowVoltageSetpoint(200.0);
        compensatorCreationInfos.setHighVoltageSetpoint(400.0);
        compensatorCreationInfos.setLowVoltageThreshold(250.0);
        compensatorCreationInfos.setHighVoltageThreshold(300.0);
        compensatorCreationInfos.setQ0(400.0);

        String compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator1' : q0 must be within the range of minimum Q and maximum Q",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setMinQAtNominalV(null);
        compensatorCreationInfos.setMaxQAtNominalV(null);
        compensatorCreationInfos.setMaxSusceptance(300.0);
        compensatorCreationInfos.setMinSusceptance(200.0);
        compensatorCreationInfos.setB0(400.0);
        compensatorCreationInfos.setQ0(null);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator1' : b0 must be within the range of minimum susceptance and maximum susceptance",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setRegulationMode(OFF);
        compensatorCreationInfos.setStandby(true);

        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("CREATE_STATIC_VAR_COMPENSATOR_ERROR : " +
                        "StaticVarCompensator 'idStaticVarCompensator1' : Standby is only supported in Voltage Regulation mode",
                compensatorCreationInfos.getErrorType().name(), reportService);
        compensatorCreationInfos.setRegulationMode(VOLTAGE);
        compensatorCreationInfos.setB0(null);
        compensatorCreationInfos.setQ0(200.0);
        compensatorCreationInfos.setLowVoltageSetpoint(200.0);
        compensatorCreationInfos.setHighVoltageSetpoint(400.0);
        compensatorCreationInfos.setLowVoltageThreshold(250.0);
        compensatorCreationInfos.setHighVoltageThreshold(300.0);
        compensatorCreationInfosJson = mapper.writeValueAsString(compensatorCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(compensatorCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("idStaticVarCompensator1", updatedValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("STATIC_VAR_COMPENSATOR_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        assertEquals("idStaticVarCompensator1Edited", updatedValues.get("equipmentId"));
    }
}
