/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.tabularcreations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.TabularCreationInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
public class TabularGeneratorCreationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).connected(true)
                .energySource(EnergySource.HYDRO).minActivePower(0).maxActivePower(100).ratedNominalPower(10D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minimumReactivePower(7D).maximumReactivePower(13D).participate(true).droop(0.5F)
                .transientReactance(5D).stepUpTransformerReactance(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).connected(false)
                .energySource(EnergySource.NUCLEAR).minActivePower(0).maxActivePower(500)
                .activePowerSetpoint(300).reactivePowerSetpoint(400D).voltageRegulationOn(false)
                .plannedActivePowerSetPoint(200D).forcedOutageRate(3D)
                .minimumReactivePower(7D).participate(false)
                .stepUpTransformerReactance(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).connected(false).connected(true)
                .energySource(EnergySource.WIND).minActivePower(0).maxActivePower(200)
                .activePowerSetpoint(150).voltageRegulationOn(true).voltageSetpoint(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).connected(false)
                .energySource(EnergySource.OTHER).minActivePower(0).maxActivePower(800)
                .activePowerSetpoint(700).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(373D)
                .marginalCost(5D).plannedOutageRate(8D)
                .participate(false)
                .transientReactance(5D)
                .regulatingTerminalId("v5load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v5").qPercent(75D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("v5generator").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("v5generator").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).connected(false).connected(true)
                .energySource(EnergySource.WIND).minActivePower(0).maxActivePower(200)
                .activePowerSetpoint(150).voltageRegulationOn(true).voltageSetpoint(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build()
        );
        return TabularCreationInfos.builder()
            .creationType(ModificationType.GENERATOR_CREATION)
            .creations(creations)
            .stashed(false)
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name11").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId11").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(200).connected(false)
                .energySource(EnergySource.SOLAR).minActivePower(0).maxActivePower(300).ratedNominalPower(20D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(80D).marginalCost(3D).plannedOutageRate(12D).forcedOutageRate(1D)
                .minimumReactivePower(13D).maximumReactivePower(11D).participate(false)
                .transientReactance(8D).stepUpTransformerReactance(44D)
                .regulatingTerminalId("v5load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v5").qPercent(37D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id4").equipmentName("name44").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId44").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(300).connected(false)
                .energySource(EnergySource.THERMAL).minActivePower(0).maxActivePower(800)
                .activePowerSetpoint(350).reactivePowerSetpoint(10D).voltageRegulationOn(false)
                .marginalCost(25D).plannedOutageRate(18D)
                .participate(true).droop(5F)
                .transientReactance(3D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(75D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build()
        );
        return TabularCreationInfos.builder()
                .creationType(ModificationType.GENERATOR_CREATION)
                .creations(creations)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getGenerator("id1"));
        assertNotNull(getNetwork().getGenerator("id2"));
        assertNotNull(getNetwork().getGenerator("id3"));
        assertNotNull(getNetwork().getGenerator("id4"));
        assertLogMessage("Tabular creation: 4 generators have been created and 1 have not been created", "tabularGENERATOR_CREATIONWarning", reportService);
        assertLogMessage("GENERATOR_ALREADY_EXISTS : v5generator", ModificationType.GENERATOR_CREATION.name() + "1", reportService);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getGenerator("id1"));
        assertNull(getNetwork().getGenerator("id2"));
        assertNull(getNetwork().getGenerator("id3"));
        assertNull(getNetwork().getGenerator("id4"));
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.GENERATOR));
    }

    @Test
    public void testCheckSqlRequestsCount() throws Exception {
        UUID modificationUuid = saveModification(buildModification());
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(8);

        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id5").equipmentName("name5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("feederId5").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).connected(true)
                .energySource(EnergySource.HYDRO).minActivePower(0).maxActivePower(100).ratedNominalPower(10D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minimumReactivePower(7D).maximumReactivePower(13D).participate(true).droop(0.5F)
                .transientReactance(5D).stepUpTransformerReactance(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build()
        );
        TabularCreationInfos creationInfos = TabularCreationInfos.builder()
                .creationType(ModificationType.GENERATOR_CREATION)
                .creations(creations)
                .build();
        modificationUuid = saveModification(creationInfos);
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(4);
        reset();

        // We get the modifications of the group (so the 2 tabular creations)
        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
                .andExpect(status().isOk());
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(12);
    }

    @Test
    public void testAllModificationsHaveSucceeded() throws Exception {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).connected(true)
                .energySource(EnergySource.HYDRO).minActivePower(0).maxActivePower(100).ratedNominalPower(10D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minimumReactivePower(7D).maximumReactivePower(13D).participate(true).droop(0.5F)
                .transientReactance(5D).stepUpTransformerReactance(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v2").busOrBusbarSectionId("1A")
                .connectionName("feederId2").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).connected(false)
                .energySource(EnergySource.NUCLEAR).minActivePower(0).maxActivePower(500)
                .activePowerSetpoint(300).reactivePowerSetpoint(400D).voltageRegulationOn(false)
                .plannedActivePowerSetPoint(200D).forcedOutageRate(3D)
                .minimumReactivePower(7D).participate(false)
                .stepUpTransformerReactance(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .build()
        );

        ModificationInfos creationInfos = TabularCreationInfos.builder()
            .creationType(ModificationType.GENERATOR_CREATION)
            .creations(creations)
            .build();
        String tabularCreationJson = mapper.writeValueAsString(creationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(tabularCreationJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        assertLogMessage("Tabular creation: 2 generators have been created", "tabularGENERATOR_CREATION", reportService);
    }

    @Test
    public void testAllModificationsHaveFailed() throws Exception {
        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id1").equipmentName("name1").voltageLevelId("unknown_vl").busOrBusbarSectionId("1.1")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).connected(true)
                .energySource(EnergySource.HYDRO).minActivePower(0).maxActivePower(100).ratedNominalPower(10D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minimumReactivePower(7D).maximumReactivePower(13D).participate(true).droop(0.5F)
                .transientReactance(5D).stepUpTransformerReactance(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id2").equipmentName("name2").voltageLevelId("v1").busOrBusbarSectionId("unknown_bbs")
                .connectionName("feederId1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).connected(true)
                .energySource(EnergySource.HYDRO).minActivePower(0).maxActivePower(100).ratedNominalPower(10D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minimumReactivePower(7D).maximumReactivePower(13D).participate(true).droop(0.5F)
                .transientReactance(5D).stepUpTransformerReactance(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").equipmentName("name3").voltageLevelId("v1").busOrBusbarSectionId("1.1")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).connected(true)
                .energySource(EnergySource.HYDRO).minActivePower(0).maxActivePower(-100).ratedNominalPower(10D)
                .activePowerSetpoint(50).reactivePowerSetpoint(20D).voltageRegulationOn(true).voltageSetpoint(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(8D).forcedOutageRate(3D)
                .minimumReactivePower(7D).maximumReactivePower(13D).participate(true).droop(0.5F)
                .transientReactance(5D).stepUpTransformerReactance(45D)
                .regulatingTerminalId("v2load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v2").qPercent(35D)
                .reactiveCapabilityCurve(false)
                .build()
        );
        ModificationInfos creationInfos = TabularCreationInfos.builder()
                .creationType(ModificationType.GENERATOR_CREATION)
                .creations(creations)
                .build();
        String tabularCreationJson = mapper.writeValueAsString(creationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(tabularCreationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                        .andExpect(status().isOk()).andReturn();
        assertLogMessage("Tabular creation: No generators have been created", "tabularGENERATOR_CREATIONError", reportService);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.GENERATOR_CREATION.name(), createdValues.get("tabularCreationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(ModificationType.TABULAR_CREATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.GENERATOR_CREATION.name(), updatedValues.get("tabularCreationType"));
    }
}
