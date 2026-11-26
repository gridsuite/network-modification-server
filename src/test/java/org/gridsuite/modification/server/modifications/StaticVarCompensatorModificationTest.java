/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.StaticVarCompensator;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
class StaticVarCompensatorModificationTest extends AbstractInjectionModificationTest {
    private static final String SVC_ID = "v3Compensator";
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return StaticVarCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId(SVC_ID)
                .equipmentName(new AttributeModification<>("newName", OperationType.SET))
                .minSusceptance(new AttributeModification<>(1., OperationType.SET))
                .maxSusceptance(new AttributeModification<>(40., OperationType.SET))
                .regulationMode(new AttributeModification<>(StaticVarCompensator.RegulationMode.REACTIVE_POWER, OperationType.SET))
                .regulating(new AttributeModification<>(true, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(390., OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(50., OperationType.SET))
                .voltageRegulationType(new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v3load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v3", OperationType.SET))
                .standbyAutomatonOn(new AttributeModification<>(true, OperationType.SET))
                .standby(new AttributeModification<>(false, OperationType.SET))
                .b0(new AttributeModification<>(5., OperationType.SET))
                .lowVoltageSetpoint(new AttributeModification<>(370., OperationType.SET))
                .highVoltageSetpoint(new AttributeModification<>(390., OperationType.SET))
                .lowVoltageThreshold(new AttributeModification<>(360., OperationType.SET))
                .highVoltageThreshold(new AttributeModification<>(400., OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return StaticVarCompensatorModificationInfos.builder()
                .stashed(false)
                .equipmentId("idCompensatorEdited")
                .equipmentName(new AttributeModification<>("nameEdited", OperationType.SET))
                .minQAtNominalV(new AttributeModification<>(10., OperationType.SET))
                .maxQAtNominalV(new AttributeModification<>(20., OperationType.SET))
                .regulating(new AttributeModification<>(false, OperationType.SET))
                .voltageRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET))
                .standbyAutomatonOn(new AttributeModification<>(false, OperationType.SET))
                .build();
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("STATIC_VAR_COMPENSATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(SVC_ID, updatedValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getStaticVarCompensator(SVC_ID), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getStaticVarCompensator(SVC_ID), true);
    }
}
