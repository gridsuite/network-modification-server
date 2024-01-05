/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.dto.TabularModificationInfos.TABULAR_EQUIPMENT_TYPE;
import static org.junit.Assert.assertEquals;

/**
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularSubstationModificationsTest extends AbstractNetworkModificationTest {
    public static final IdentifiableType EQUIPMENT_TYPE = IdentifiableType.SUBSTATION;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {

        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).substationCountry(new AttributeModification<>(Country.BE, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).substationCountry(new AttributeModification<>(Country.BE, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .equipmentType(EQUIPMENT_TYPE)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).substationCountry(new AttributeModification<>(Country.JP, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).substationCountry(new AttributeModification<>(Country.JP, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .equipmentType(EQUIPMENT_TYPE)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(Country.BE, getNetwork().getSubstation("s1").getCountry().orElse(Country.AF));
        assertEquals("s1", getNetwork().getSubstation("s1").getOptionalName().orElse("s2"));
        assertEquals(Country.BE, getNetwork().getSubstation("s2").getCountry().orElse(Country.AF));
        assertEquals("s2", getNetwork().getSubstation("s2").getOptionalName().orElse("s1"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(Country.FR, getNetwork().getSubstation("s1").getCountry().orElse(Country.BE));
        assertEquals("s1", getNetwork().getSubstation("s1").getOptionalName().orElse("s2"));
        assertEquals(Country.FR, getNetwork().getSubstation("s2").getCountry().orElse(Country.BE));
        assertEquals("s2", getNetwork().getSubstation("s2").getOptionalName().orElse("s1"));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(EQUIPMENT_TYPE.name(), createdValues.get(TABULAR_EQUIPMENT_TYPE));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("TABULAR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(EQUIPMENT_TYPE.name(), updatedValues.get(TABULAR_EQUIPMENT_TYPE));
    }
}
