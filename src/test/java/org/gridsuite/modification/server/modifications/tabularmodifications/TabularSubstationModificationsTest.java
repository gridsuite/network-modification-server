/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularmodifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author AJELLAL Ali <ali.ajellal@rte-france.com>
 */
@Tag("IntegrationTest")
class TabularSubstationModificationsTest extends AbstractNetworkModificationTest {
    public static final ModificationType MOFIFICATION_TYPE = ModificationType.SUBSTATION_MODIFICATION;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {

        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).country(new AttributeModification<>(Country.BE, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).country(new AttributeModification<>(Country.BE, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(MOFIFICATION_TYPE)
                .modifications(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                SubstationModificationInfos.builder().equipmentId("s1").equipmentName(new AttributeModification<>("s1", OperationType.SET)).country(new AttributeModification<>(Country.JP, OperationType.SET)).build(),
                SubstationModificationInfos.builder().equipmentId("s2").equipmentName(new AttributeModification<>("s2", OperationType.SET)).country(new AttributeModification<>(Country.JP, OperationType.SET)).build()
        );
        return TabularModificationInfos.builder()
                .modificationType(MOFIFICATION_TYPE)
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
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(MOFIFICATION_TYPE.name(), createdValues.get("tabularModificationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(MOFIFICATION_TYPE.name(), updatedValues.get("tabularModificationType"));
    }
}
