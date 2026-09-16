/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.tabularcreations;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.GeneratorCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularPropertyInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

/**
 * Checks (1) that the SQL request count to read a tabular creation does not depend on the number of its
 * sub-creations (the JPA N+1 problem is correctly solved), and (2) that a bulk tabular creation produces a single
 * server-side {@code CollectionElementImpact} per created element type rather than one impact per sub-creation.
 *
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
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(0.5).forcedOutageRate(0.3)
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
                .plannedActivePowerSetPoint(200D).forcedOutageRate(0.3)
                .minQ(7D).participate(false)
                .stepUpTransformerX(45D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id3").voltageLevelId("v3").busOrBusbarSectionId("3A")
                .connectionName("feederId3").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.WIND).minP(0).maxP(200)
                .targetP(150).voltageRegulationOn(true).targetV(375D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id4").equipmentName("name4").voltageLevelId("v4").busOrBusbarSectionId("1.A")
                .connectionName("feederId4").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(false)
                .energySource(EnergySource.OTHER).minP(0).maxP(800)
                .targetP(700).targetQ(20D).voltageRegulationOn(true).targetV(373D)
                .marginalCost(5D).plannedOutageRate(0.5)
                .participate(false)
                .directTransX(5D)
                .regulatingTerminalId("v5load").regulatingTerminalType("LOAD").regulatingTerminalVlId("v5").qPercent(75D)
                .reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(null)
                .build(),
            GeneratorCreationInfos.builder()
                .equipmentId("id5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("name5").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(100).terminalConnected(true)
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
                .plannedActivePowerSetPoint(80D).marginalCost(3D).plannedOutageRate(0.6).forcedOutageRate(0.1)
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
                .marginalCost(25D).plannedOutageRate(0.4)
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
        assertSelectCount(6);

        List<ModificationInfos> creations = List.of(
            GeneratorCreationInfos.builder()
                .equipmentId("id5").equipmentName("name5").voltageLevelId("v5").busOrBusbarSectionId("1A1")
                .connectionName("feederId5").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(100).terminalConnected(true)
                .energySource(EnergySource.HYDRO).minP(0).maxP(100).ratedS(10D)
                .targetP(50).targetQ(20D).voltageRegulationOn(true).targetV(370D)
                .plannedActivePowerSetPoint(70D).marginalCost(5D).plannedOutageRate(0.7).forcedOutageRate(0.3)
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
        assertSelectCount(6);
        reset();

        // We get the modifications of the group (so the 2 tabular creations)
        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
                .andExpect(status().isOk());
        // We check that the request count is not dependent on the number of sub creations of the tabular creation (the JPA N+1 problem is correctly solved)
        assertSelectCount(10);
    }
}
