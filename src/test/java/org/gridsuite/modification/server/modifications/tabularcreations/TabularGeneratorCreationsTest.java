/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.GeneratorCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.dto.tabular.*;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
class TabularGeneratorCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.NUCLEAR).minP(0).maxP(500)
                .targetP(300).targetQ(400D).voltageRegulationOn(false)
                .plannedActivePowerSetPoint(200D).forcedOutageRate(3D)
                .minQ(7D).participate(false)
                .stepUpTransformerX(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.OTHER).minP(0).maxP(800)
                .targetP(700).targetQ(20D).voltageRegulationOn(true).targetV(373D)
                .marginalCost(5D).plannedOutageRate(8D)
                .participate(false)
                .directTransX(5D)
                .regulatingTerminalId("v5load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v5").qPercent(75D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("name5").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("v5generator").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v5generator").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build()
        );
        return TabularCreationInfos.builder()
            .modificationType(ModificationType.GENERATOR_CREATION)
            .modifications(creations)
            .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(true).build()))
            .stashed(false)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name11").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId11").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(200).terminalConnected(false)
                .energySource(EnergySource.SOLAR).minP(0).maxP(300).ratedS(20D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(80D).marginalCost(3D).plannedOutageRate(12D).forcedOutageRate(1D)
                .minQ(13D).maxQ(11D).participate(false)
                .directTransX(8D).stepUpTransformerX(44D)
                .regulatingTerminalId("v5load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v5").qPercent(37D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id4").equipmentName("name44").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId44").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(300).terminalConnected(false)
                .energySource(EnergySource.THERMAL).minP(0).maxP(800)
                .targetP(350).targetQ(10D).voltageRegulationOn(false)
                .marginalCost(25D).plannedOutageRate(18D)
                .participate(true).droop(5F)
                .directTransX(3D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(75D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build()
        );
        return TabularCreationInfos.builder()
                .modificationType(ModificationType.GENERATOR_CREATION)
                .modifications(creations)
                .properties(List.of(TabularPropertyInfos.builder().name("P1").predefined(true).selected(false).build()))
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getGenerator("id1"));
        assertNotNull(getNetwork().getGenerator("id2"));
        assertNotNull(getNetwork().getGenerator("id3"));
        assertNotNull(getNetwork().getGenerator("id4"));
        assertNotNull(getNetwork().getGenerator("id5"));
        assertLogMessage("Tabular creation: 5 generators have been created and 1 have not been created", "network.modification.tabular.creation.warning", reportService);
        assertLogMessage("GENERATOR_ALREADY_EXISTS : v5generator", "network.modification.tabular.creation.exception", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getGenerator("id1"));
        assertNull(getNetwork().getGenerator("id2"));
        assertNull(getNetwork().getGenerator("id3"));
        assertNull(getNetwork().getGenerator("id4"));
        assertNull(getNetwork().getGenerator("id5"));
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.SWITCH), createCollectionElementImpact(IdentifiableType.GENERATOR));
    }

    @Test
    void testCheckSqlRequestsCount() throws Exception {
        UUID modificationUuid = saveModification(buildModification());
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(5);

        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id5").equipmentName("name5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("feederId5").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build()
        );
        TabularCreationInfos creationInfos = TabularCreationInfos.builder()
                .modificationType(ModificationType.GENERATOR_CREATION)
                .modifications(creations)
                .build();
        modificationUuid = saveModification(creationInfos);
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(5);
        reset();

        // We get the modifications of the group (so the 2 tabular creations)
        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
                .andExpect(status().isOk());
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(10);
    }

    @Test
    void testAllModificationsHaveSucceeded() throws Exception {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.NUCLEAR).minP(0).maxP(500)
                .targetP(300).targetQ(400D).voltageRegulationOn(false)
                .plannedActivePowerSetPoint(200D).forcedOutageRate(3D)
                .minQ(7D).participate(false)
                .stepUpTransformerX(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build()
        );

        ModificationInfos creationInfos = TabularCreationInfos.builder()
            .modificationType(ModificationType.GENERATOR_CREATION)
            .modifications(creations)
            .build();
        String tabularCreationJson = getJsonBody(creationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(tabularCreationJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertLogMessage("Tabular creation: 2 generators have been created", "network.modification.tabular.creation", reportService);
    }

    @Test
    void testAllModificationsHaveFailed() throws Exception {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("unknown_vl").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v1").busOrBusbarSectionId("unknown_bbs")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").equipmentName("name3").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(-100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minQ(7D).maxQ(13D).participate(true).droop(0.5F)
                .directTransX(5D).stepUpTransformerX(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .modificationType(ModificationType.GENERATOR_CREATION)
                .modifications(creations)
                .build();
        String tabularCreationJson = getJsonBody(creationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(tabularCreationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();
        assertLogMessage("Tabular creation: No generators have been created", "network.modification.tabular.creation.error", reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.GENERATOR_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(ModificationType.GENERATOR_CREATION.name(), updatedValues.get("tabularCreationType"));
    }

    @Test
    void testUnsupportedTabularCreationType() throws Exception {
        List<ModificationInfos> creations = List.of(
                SubstationCreationInfos.builder()
                        .stashed(false)
                        .equipmentId("SubstationId")
                        .equipmentName("SubstationName")
                        .country(Country.AF)
                        .properties(List.of(FreePropertyInfos.builder().name("DEMO").value("DemoC").build()))
                        .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .modificationType(ModificationType.SUBSTATION_CREATION)
                .modifications(creations)
                .build();
        String tabularCreationJson = getJsonBody(creationInfos, null);

        // creation
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(tabularCreationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        NetworkModificationsResult result = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(result);
        assertEquals(1, result.modificationUuids().size());
        UUID modifId = result.modificationUuids().get(0);

        // try to get via the group
        UnsupportedOperationException exception = assertThrows(
                UnsupportedOperationException.class,
            () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
        assertEquals("No sub-modifications loading for creation type: SUBSTATION_CREATION", exception.getMessage());

        // try to get via id
        exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.getModificationInfo(modifId)
        );
        assertEquals("No sub-modifications loading for creation type: SUBSTATION_CREATION", exception.getMessage());

        // try to update
        exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.updateModification(modifId, creationInfos)
        );
        // deletion error because we try to remove the sub-modifications before updating them
        assertEquals("No sub-modifications deletion method for type: SUBSTATION_CREATION", exception.getMessage());

        // try to delete
        List<UUID> ids = List.of(modifId);
        exception = assertThrows(
                UnsupportedOperationException.class,
                () -> networkModificationRepository.deleteModifications(TEST_GROUP_ID, ids)
        );
        assertEquals("No sub-modifications deletion method for type: SUBSTATION_CREATION", exception.getMessage());
    }
}
