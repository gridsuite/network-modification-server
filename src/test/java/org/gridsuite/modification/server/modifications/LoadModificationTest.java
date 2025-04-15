/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.Measurement;
import com.powsybl.iidm.network.extensions.Measurements;

import org.apache.commons.collections4.CollectionUtils;
import org.assertj.core.api.Assertions;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class LoadModificationTest extends AbstractInjectionModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double MEASUREMENT_P_VALUE = 10.0;
    private static final Double MEASUREMENT_Q_VALUE = -10.0;
    private static final Boolean MEASUREMENT_P_VALID = true;
    private static final Boolean MEASUREMENT_Q_VALID = false;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadModificationInfos.builder()
            .stashed(false)
            .equipmentId("v1load")
            .equipmentName(new AttributeModification<>("nameLoad1", OperationType.SET))
            .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
            .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
            .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
            .p0(new AttributeModification<>(200.0, OperationType.SET))
            .q0(new AttributeModification<>(30.0, OperationType.SET))
            .pMeasurementValue(new AttributeModification<>(MEASUREMENT_P_VALUE, OperationType.SET))
            .pMeasurementValidity(new AttributeModification<>(MEASUREMENT_P_VALID, OperationType.SET))
            .qMeasurementValue(new AttributeModification<>(MEASUREMENT_Q_VALUE, OperationType.SET))
            .qMeasurementValidity(new AttributeModification<>(MEASUREMENT_Q_VALID, OperationType.SET))
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadModificationInfos.builder()
            .equipmentId("v1loadEdited")
            .stashed(false)
            .equipmentName(new AttributeModification<>("nameLoadEdited1", OperationType.SET))
            .loadType(new AttributeModification<>(LoadType.AUXILIARY, OperationType.SET))
            .p0(new AttributeModification<>(300.0, OperationType.SET))
            .q0(new AttributeModification<>(50.0, OperationType.SET))
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Load modifiedLoad = getNetwork().getLoad("v1load");
        assertNotNull(modifiedLoad);
        assertEquals(LoadType.FICTITIOUS, modifiedLoad.getLoadType());
        assertEquals(200.0, modifiedLoad.getP0(), 0.0);
        assertEquals(30.0, modifiedLoad.getQ0(), 0.0);
        assertEquals("nameLoad1", modifiedLoad.getNameOrId());
        assertEquals(PROPERTY_VALUE, modifiedLoad.getProperty(PROPERTY_NAME));
        assertMeasurements(modifiedLoad);
    }

    private void assertMeasurements(Load load) {
        Measurements<?> measurements = (Measurements<?>) load.getExtension(Measurements.class);
        assertNotNull(measurements);
        Collection<Measurement> activePowerMeasurements = measurements.getMeasurements(Measurement.Type.ACTIVE_POWER).stream().toList();
        assertNotNull(activePowerMeasurements);
        assertFalse(CollectionUtils.isEmpty(activePowerMeasurements));
        Assertions.assertThat(activePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_P_VALUE && m.isValid() == MEASUREMENT_P_VALID);
        Collection<Measurement> reactivePowerMeasurements = measurements.getMeasurements(Measurement.Type.REACTIVE_POWER).stream().toList();
        assertFalse(CollectionUtils.isEmpty(reactivePowerMeasurements));
        Assertions.assertThat(reactivePowerMeasurements).allMatch(m -> m.getValue() == MEASUREMENT_Q_VALUE && m.isValid() == MEASUREMENT_Q_VALID);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        Load modifiedLoad = getNetwork().getLoad("v1load");
        assertNotNull(modifiedLoad);
        assertEquals(LoadType.UNDEFINED, modifiedLoad.getLoadType());
        assertEquals(0.0, modifiedLoad.getP0(), 0.0);
        assertEquals(0.0, modifiedLoad.getQ0(), 0.0);
        assertEquals("v1load", modifiedLoad.getNameOrId());
        assertNull(modifiedLoad.getProperty(PROPERTY_NAME));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // Unset an attribute that should not be null
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();
        String loadModificationInfosJson = getJsonBody(loadModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(loadModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Load 'v1load': load type is null", loadModificationInfos.getErrorType().name(), reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LOAD_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1load", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LOAD_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1loadEdited", updatedValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getLoad("v1load"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getLoad("v1load"), true);
    }
}
