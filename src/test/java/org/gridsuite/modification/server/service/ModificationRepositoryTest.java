/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MOVE_MODIFICATION_ERROR;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.Assert.*;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@DisableElasticsearch
@Tag("IntegrationTest")
public class ModificationRepositoryTest {
    private static final UUID TEST_GROUP_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID_2 = UUID.fromString("5809dabf-60f8-46e5-9e58-57b03d6b1818");
    private static final UUID TEST_GROUP_ID_3 = UUID.fromString("de67bab1-f47b-4199-80a7-10bd77285675");

    @Autowired
    private ModificationGroupRepository modificationGroupRepository;

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    @Autowired
    private ModificationRepository modificationRepository;

    @Before
    public void setUp() {
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
        SQLStatementCountValidator.reset();
    }

    public EquipmentAttributeModificationInfos getEquipmentAttributeModification(UUID modificationUuid) {
        return (EquipmentAttributeModificationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private LoadCreationInfos getLoadCreationModification(UUID modificationUuid) {
        return (LoadCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private GeneratorCreationInfos getGeneratorCreationModification(UUID modificationUuid) {
        return (GeneratorCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private LineCreationInfos getLineCreationModification(UUID modificationUuid) {
        return (LineCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private SubstationCreationInfos getSubstationCreationModification(UUID modificationUuid) {
        return (SubstationCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private VoltageLevelCreationInfos getVoltageLevelCreationModification(UUID modificationUuid) {
        return (VoltageLevelCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    public GroovyScriptInfos getGroovyScript(UUID modificationUuid) {
        return (GroovyScriptInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    public ShuntCompensatorCreationInfos getShuntCompensatorCreationModification(UUID modificationUuid) {
        return (ShuntCompensatorCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private LineSplitWithVoltageLevelInfos getLineSplitWithVoltageLevelModification(UUID modificationUuid) {
        return (LineSplitWithVoltageLevelInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private LineAttachToVoltageLevelInfos getLineAttachToVoltageLevelModification(UUID modificationUuid) {
        return (LineAttachToVoltageLevelInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private LinesAttachToSplitLinesInfos getLinesAttachToSplitLinesModification(UUID modificationUuid) {
        return (LinesAttachToSplitLinesInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private DeleteVoltageLevelOnLineInfos getDeleteVoltageLevelOnLineModification(UUID modificationUuid) {
        return (DeleteVoltageLevelOnLineInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private VoltageInitModificationInfos getVoltageInitModification(UUID modificationUuid) {
        return (VoltageInitModificationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    @Test
    public void test() {
        assertEquals(List.of(), this.networkModificationRepository.getModificationGroupsUuids());
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
        assertEquals(0, networkModificationRepository.getModifications(TEST_GROUP_ID, true, false).size());

        var nullModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id0").equipmentAttributeName("attribute").equipmentAttributeValue(null).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var stringModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id1").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var boolModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue(true).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var intModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id3").equipmentAttributeName("attribute").equipmentAttributeValue(1).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var floatModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id4").equipmentAttributeName("attribute").equipmentAttributeValue(2F).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var doubleModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id5").equipmentAttributeName("attribute").equipmentAttributeValue(3D).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var enumModifEntity = EquipmentAttributeModificationInfos.builder().equipmentId("id6").equipmentAttributeName("attribute").equipmentAttributeValue(SwitchKind.BREAKER).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(nullModifEntity, stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity, enumModifEntity));

        List<ModificationInfos> modificationEntities = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(7, modificationEntities.size());

        // Order is also checked
        assertThat(getEquipmentAttributeModification(modificationEntities.get(0).getUuid()))
            .recursivelyEquals(nullModifEntity.toModificationInfos());
        assertThat(getEquipmentAttributeModification(modificationEntities.get(1).getUuid()))
            .recursivelyEquals(stringModifEntity.toModificationInfos());
        assertThat(getEquipmentAttributeModification(modificationEntities.get(2).getUuid()))
            .recursivelyEquals(boolModifEntity.toModificationInfos());
        assertThat(getEquipmentAttributeModification(modificationEntities.get(3).getUuid()))
            .recursivelyEquals(intModifEntity.toModificationInfos());
        assertThat(getEquipmentAttributeModification(modificationEntities.get(4).getUuid()))
            .recursivelyEquals(floatModifEntity.toModificationInfos());
        assertThat(getEquipmentAttributeModification(modificationEntities.get(5).getUuid()))
            .recursivelyEquals(doubleModifEntity.toModificationInfos());
        assertThat(getEquipmentAttributeModification(modificationEntities.get(6).getUuid()))
            .recursivelyEquals(enumModifEntity.toModificationInfos());

        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of());
        assertEquals(7, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(stringModifEntity.getId(), boolModifEntity.getId()));
        assertEquals(5, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertEquals(0, modificationRepository.findAll().size());
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testCreateModificationGroupQueryCount() {
        modificationGroupRepository.save(new ModificationGroupEntity(TEST_GROUP_ID));

        // No select
        assertRequestsCount(0, 1, 0, 0);
    }

    @Test
    public void testCreateModificationQueryCount() {
        var modifEntity1 = EquipmentAttributeModificationInfos.builder().equipmentId("id1").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var modifEntity2 = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        assertRequestsCount(1, 3, 1, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        var modifEntity1 = EquipmentAttributeModificationInfos.builder().equipmentId("id1").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var modifEntity2 = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var modifEntity3 = EquipmentAttributeModificationInfos.builder().equipmentId("id3").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2, modifEntity3));

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        getEquipmentAttributeModification(modifEntity1.getId());
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> getEquipmentAttributeModification(TEST_GROUP_ID)
        );
    }

    @Test
    public void testDeleteModificationQueryCount() {
        var modifEntity1 = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var modifEntity2 = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(modifEntity1.getId()));
        assertRequestsCount(3, 0, 1, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(3, 0, 0, 3);

        // Non-existent group modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testLoadCreation() {
        var createLoadEntity1 = LoadCreationInfos.builder().equipmentId("idLoad1").equipmentName("nameLoad1").loadType(LoadType.AUXILIARY).voltageLevelId("vlId1").busOrBusbarSectionId("busId1").p0(100.).q0(20.).connectionName("top1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(11).build().toEntity();
        var createLoadEntity2 = LoadCreationInfos.builder().equipmentId("idLoad2").equipmentName("nameLoad2").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId2").busOrBusbarSectionId("busId2").p0(80.).q0(30.).connectionName("bottom1").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(11).build().toEntity();
        var createLoadEntity3 = LoadCreationInfos.builder().equipmentId("idLoad3").equipmentName("nameLoad3").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId3").busOrBusbarSectionId("busId3").p0(50.).q0(90.).connectionName("top2").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(12).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLoadEntity1, createLoadEntity2, createLoadEntity3));
        assertRequestsCount(1, 3, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getLoadCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createLoadEntity1.toModificationInfos());
        assertThat(getLoadCreationModification(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(createLoadEntity2.toModificationInfos());
        assertThat(getLoadCreationModification(modificationInfos.get(2).getUuid()))
            .recursivelyEquals(createLoadEntity3.toModificationInfos());

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createLoadEntity2.getId(), createLoadEntity3.getId()));
        assertRequestsCount(4, 0, 1, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(3, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testGeneratorCreation() {
        var createGeneratorEntity1 = GeneratorCreationInfos.builder()
                .equipmentId("idGenerator1").equipmentName("nameGenerator1")
                .energySource(EnergySource.HYDRO).voltageLevelId("vlId1")
                .busOrBusbarSectionId("busId1").minP(100.0)
                .maxP(800.0).ratedS(10.)
                .targetP(500).targetQ(50.)
                .voltageRegulationOn(true).targetV(225.)
                .plannedActivePowerSetPoint(20.)
                .marginalCost(20.)
                .plannedOutageRate(20.).forcedOutageRate(20.)
                .minQ(30.).maxQ(50.)
                .participate(true).droop(8f).directTransX(37.)
                .stepUpTransformerX(46.).regulatingTerminalId("testTerminalId1")
                .regulatingTerminalType("LINE").regulatingTerminalVlId("idVlTest1")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(1).build().toEntity();
        var createGeneratorEntity2 = GeneratorCreationInfos.builder()
                .equipmentId("idGenerator2").equipmentName("nameGenerator2")
                .energySource(EnergySource.SOLAR).voltageLevelId("vlId2")
                .busOrBusbarSectionId("busId2").minP(0.0)
                .maxP(300.0).ratedS(5.)
                .targetP(150).targetQ(30.)
                .voltageRegulationOn(false).targetV(380.)
                .plannedActivePowerSetPoint(30.)
                .marginalCost(30.)
                .plannedOutageRate(30.).forcedOutageRate(30.)
                .participate(false).droop(null).directTransX(37.)
                .stepUpTransformerX(46.).regulatingTerminalId(null)
                .regulatingTerminalType(null).regulatingTerminalVlId("idVlTest2")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Bot").connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition(2).build().toEntity();

        var createGeneratorEntity3 = GeneratorCreationInfos.builder()
                .equipmentId("idGenerator3").equipmentName("nameGenerator3")
                .energySource(EnergySource.OTHER).voltageLevelId("vlId3")
                .busOrBusbarSectionId("busId3").minP(10.0)
                .maxP(900.0).ratedS(20.)
                .voltageRegulationOn(true).targetV(150.).marginalCost(null)
                .participate(false).droop(null).directTransX(null)
                .stepUpTransformerX(null).regulatingTerminalId("testTerminalId2")
                .regulatingTerminalType("BATTERY").regulatingTerminalVlId("idVlTest2")
                .qPercent(25.).reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(new ReactiveCapabilityCurveCreationInfos(33., 44., 55.)))
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(3).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createGeneratorEntity1, createGeneratorEntity2, createGeneratorEntity3));
        assertRequestsCount(1, 4, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getGeneratorCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createGeneratorEntity1.toModificationInfos());
        assertThat(getGeneratorCreationModification(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(createGeneratorEntity2.toModificationInfos());
        assertThat(getGeneratorCreationModification(modificationInfos.get(2).getUuid()))
            .recursivelyEquals(createGeneratorEntity3.toModificationInfos());

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createGeneratorEntity2.getId(), createGeneratorEntity3.getId()));
        assertRequestsCount(4, 0, 1, 3);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(3, 0, 0, 4);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testShuntCompensatorCreation() {
        var shunt1 = ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt1").equipmentName("nameOne")
            .maximumSectionCount(2)
            .sectionCount(1)
            .maxSusceptance(1.)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();
        var shunt2 = ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt2").equipmentName("notNameOne")
            .maximumSectionCount(2)
            .sectionCount(0)
            .maxSusceptance(1.)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();

        var createShuntCompensatorEntity1 = shunt1.toEntity();
        var createShuntCompensatorEntity2 = shunt2.toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createShuntCompensatorEntity1, createShuntCompensatorEntity2));
        assertRequestsCount(1, 3, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getShuntCompensatorCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createShuntCompensatorEntity1.toModificationInfos());
        assertThat(getShuntCompensatorCreationModification(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(createShuntCompensatorEntity2.toModificationInfos());

        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createShuntCompensatorEntity2.getId()));
        assertRequestsCount(3, 0, 1, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(3, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testLineCreation() {
        var createLineEntity1 = LineCreationInfos.builder().equipmentId("idLine1").equipmentName("nameLine1").r(1.0).x(1.1).g1(10.0).b1(11.0).g2(100.0).b2(100.1).voltageLevelId1("vlId11").busOrBusbarSectionId1("busId11").voltageLevelId2("vlId12").busOrBusbarSectionId2("busId12").connectionName1("cn11").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn22").connectionDirection2(ConnectablePosition.Direction.TOP).build().toEntity();
        var createLineEntity2 = LineCreationInfos.builder().equipmentId("idLine2").equipmentName("nameLine2").r(2.0).x(2.2).g1(20.0).b1(22.0).g2(200.0).b2(200.2).voltageLevelId1("vlId21").busOrBusbarSectionId1("busId21").voltageLevelId2("vlId22").busOrBusbarSectionId2("busId22").connectionName1("cn33").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn44").connectionDirection2(ConnectablePosition.Direction.BOTTOM).currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build()).build().toEntity();
        var createLineEntity3 = LineCreationInfos.builder().equipmentId("idLine3").equipmentName("nameLine3").r(3.0).x(3.3).g1(30.0).b1(33.0).g2(300.0).b2(300.3).voltageLevelId1("vlId31").busOrBusbarSectionId1("busId31").voltageLevelId2("vlId32").busOrBusbarSectionId2("busId32").connectionName1("cn55").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn66").connectionDirection2(ConnectablePosition.Direction.TOP).currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build()).build().toEntity();
        var createLineEntity4 = LineCreationInfos.builder().equipmentId("idLine4").equipmentName("nameLine4").r(3.0).x(3.3).g1(null).b1(null).g2(null).b2(null).voltageLevelId1("vlId41").busOrBusbarSectionId1("busId41").voltageLevelId2("vlId42").busOrBusbarSectionId2("busId42").connectionName1("cn77").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn88").connectionDirection2(ConnectablePosition.Direction.BOTTOM).currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build()).currentLimits2(CurrentLimitsInfos.builder().permanentLimit(4.0).temporaryLimits(Collections.emptyList()).build()).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLineEntity1, createLineEntity2, createLineEntity3, createLineEntity4));
        assertRequestsCount(1, 4, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(4, modificationInfos.size());

        assertThat(getLineCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createLineEntity1.toModificationInfos());
        assertThat(getLineCreationModification(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(createLineEntity2.toModificationInfos());
        assertThat(getLineCreationModification(modificationInfos.get(2).getUuid()))
            .recursivelyEquals(createLineEntity3.toModificationInfos());
        assertThat(getLineCreationModification(modificationInfos.get(3).getUuid()))
            .recursivelyEquals(createLineEntity4.toModificationInfos());

        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createLineEntity2.getId(), createLineEntity3.getId()));
        assertRequestsCount(6, 0, 1, 7);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(8, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(6, 0, 0, 5);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testMoveModificationInSameGroup() {
        // use a group Tabular modification
        List<ModificationInfos> groupModifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("unknownGenerator").maxActivePower(new AttributeModification<>(500., OperationType.SET)).build()
        );
        ModificationEntity tabularModificationEntity = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(groupModifications)
                .stashed(false)
                .build()
                .toEntity();
        // and 5 script modifications
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build().toEntity();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build().toEntity();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build().toEntity();
        var groovyScriptEntity4 = GroovyScriptInfos.builder().script("script4").build().toEntity();
        var groovyScriptEntity5 = GroovyScriptInfos.builder().script("script5").build().toEntity();

        List<ModificationEntity> modificationEntities = List.of(groovyScriptEntity1, groovyScriptEntity2, groovyScriptEntity3, groovyScriptEntity4, groovyScriptEntity5, tabularModificationEntity);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, modificationEntities);
        assertRequestsCount(1, 8, 1, 0);

        var modificationOriginal = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, TEST_GROUP_ID, List.of(tabularModificationEntity.getId()), groovyScriptEntity2.getId());
        assertRequestsCount(2, 0, 1, 0);

        var modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        // [0:1, 1:6, 2:2, 3:3, 4:4 ,5:5 ]
        var expected = List.of(modificationOriginal.get(0), modificationOriginal.get(5),
            modificationOriginal.get(1), modificationOriginal.get(2), modificationOriginal.get(3), modificationOriginal.get(4));

        assertEquals(getIds(expected), getIds(modification));

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, TEST_GROUP_ID, List.of(groovyScriptEntity3.getId(), tabularModificationEntity.getId()), null);
        assertRequestsCount(2, 0, 1, 0);

        // [0:1, 1:2, 2:4, 3:5, 4:6, 5:3 ]
        modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        expected = List.of(modificationOriginal.get(0), modificationOriginal.get(1), modificationOriginal.get(3),
            modificationOriginal.get(4), modificationOriginal.get(2), modificationOriginal.get(5));
        assertEquals(getIds(expected), getIds(modification));

    }

    @Test
    public void testMoveModificationsBetweenTwoGroups() {
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build().toEntity();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build().toEntity();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build().toEntity();
        var groovyScriptEntity4 = GroovyScriptInfos.builder().script("script4").build().toEntity();
        var groovyScriptEntity5 = GroovyScriptInfos.builder().script("script5").build().toEntity();
        var groovyScriptEntity6 = GroovyScriptInfos.builder().script("script6").build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2,
                groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(1, 3, 1, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 3, 1, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        List<UUID> uuidsToMove = List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId());
        List<ModificationEntity> movedModifications = networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, uuidsToMove, null);
        assertEquals(uuidsToMove.size(), movedModifications.size());
        assertRequestsCount(4, 0, 2, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        var expected1 = List.of(modificationOriginal1.get(0), modificationOriginal1.get(3));
        var expected2 = List.of(modificationOriginal2.get(0), modificationOriginal2.get(1), modificationOriginal1.get(1), modificationOriginal1.get(2));

        assertEquals(getIds(expected1), getIds(modification1));
        assertEquals(getIds(expected2), getIds(modification2));

        // cutting and pasting to non existing group should work (the destination group is implicitly created)
        SQLStatementCountValidator.reset();
        uuidsToMove = List.of(expected2.get(0).getUuid(), expected2.get(1).getUuid());
        movedModifications = networkModificationRepository.moveModifications(TEST_GROUP_ID_3, TEST_GROUP_ID_2, uuidsToMove, null);
        assertEquals(uuidsToMove.size(), movedModifications.size());
        assertRequestsCount(3, 1, 3, 0);

        modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modification3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        expected2 = List.of(modificationOriginal1.get(1), modificationOriginal1.get(2));
        var expected3 = List.of(modificationOriginal2.get(0), modificationOriginal2.get(1));

        assertEquals(getIds(modification2), getIds(expected2));
        assertEquals(getIds(expected3), getIds(modification3));
    }

    @Test
    public void testMoveModificationsBetweenTwoGroupsWithReferenceNode() {
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build().toEntity();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build().toEntity();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build().toEntity();
        var groovyScriptEntity4 = GroovyScriptInfos.builder().script("script4").build().toEntity();
        var groovyScriptEntity5 = GroovyScriptInfos.builder().script("script5").build().toEntity();
        var groovyScriptEntity6 = GroovyScriptInfos.builder().script("script6").build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2,
                groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(1, 3, 1, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 3, 1, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        List<UUID> uuidsToMove = List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId());
        List<ModificationEntity> movedModifications = networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, uuidsToMove, groovyScriptEntity6.getId());
        assertEquals(uuidsToMove.size(), movedModifications.size());
        assertRequestsCount(4, 0, 2, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        var expected1 = List.of(modificationOriginal1.get(0), modificationOriginal1.get(3));
        var expected2 = List.of(modificationOriginal2.get(0), modificationOriginal1.get(1), modificationOriginal1.get(2), modificationOriginal2.get(1));

        assertEquals(getIds(expected1), getIds(modification1));
        assertEquals(getIds(expected2), getIds(modification2));
    }

    @Test
    public void testMoveModificationsBetweenMoreThanTwoGroups() {
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build().toEntity();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build().toEntity();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build().toEntity();
        var groovyScriptEntity4 = GroovyScriptInfos.builder().script("script4").build().toEntity();
        var groovyScriptEntity5 = GroovyScriptInfos.builder().script("script5").build().toEntity();
        var groovyScriptEntity6 = GroovyScriptInfos.builder().script("script6").build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2));
        assertRequestsCount(1, 3, 1, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(1, 3, 1, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 3, 1, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modificationOriginal3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        // moving modifications with a bad modification should work (the bad one will be ignored)
        SQLStatementCountValidator.reset();
        List<UUID> modificationsToMoveUuid = List.of(groovyScriptEntity1.getId(), UUID.randomUUID());
        List<ModificationEntity> movedModifications = networkModificationRepository.moveModifications(TEST_GROUP_ID_3, TEST_GROUP_ID, modificationsToMoveUuid, null);
        assertRequestsCount(4, 0, 2, 0);
        // only the valid modification is moved
        assertEquals(1, movedModifications.size());
        assertEquals(groovyScriptEntity1.getId(), movedModifications.get(0).getId());

        // moving modification with reference node not in destination: exception expected
        SQLStatementCountValidator.reset();
        List <UUID> modificationsToMoveUuid2 = List.of(groovyScriptEntity1.getId());
        UUID referenceNodeUuid = groovyScriptEntity2.getId();
        assertThrows(new NetworkModificationException(MOVE_MODIFICATION_ERROR).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, modificationsToMoveUuid2, referenceNodeUuid)
        );
        assertRequestsCount(4, 0, 0, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modification3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        var expected1 = List.of(modificationOriginal1.get(1));
        var expected3 = List.of(modificationOriginal3.get(0), modificationOriginal3.get(1), modificationOriginal1.get(0));

        assertEquals(getIds(modification1), getIds(expected1));
        assertEquals(getIds(modification2), getIds(modificationOriginal2));
        assertEquals(getIds(modification3), getIds(expected3));
    }

    private List<UUID> getIds(List<ModificationInfos> expected) {
        return expected.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
    }

    @Test
    public void testGroovyScript() {
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build().toEntity();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build().toEntity();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2, groovyScriptEntity3));
        assertRequestsCount(1, 3, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getGroovyScript(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(groovyScriptEntity1.toModificationInfos());
        assertThat(getGroovyScript(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(groovyScriptEntity2.toModificationInfos());
        assertThat(getGroovyScript(modificationInfos.get(2).getUuid()))
            .recursivelyEquals(groovyScriptEntity3.toModificationInfos());

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId()));
        assertRequestsCount(2, 0, 1, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testSubstationCreation() {
        var createSubstationEntity1 = SubstationCreationInfos.builder()
                .equipmentId("idSubstation1")
                .equipmentName("nameSubstation1")
                .substationCountry(Country.FR)
                .properties(List.of(FreePropertyInfos.builder().name("DEMO").value("DemoU").build()))
                .build().toEntity();
        var createSubstationEntity2 = SubstationCreationInfos.builder()
                .equipmentId("idSubstation2")
                .equipmentName("nameSubstation2")
                .substationCountry(Country.TD)
                .properties(null)
                .build().toEntity();
        var createSubstationEntity3 = SubstationCreationInfos.builder()
                .equipmentId("idSubstation3")
                .equipmentName("nameSubstation3")
                .substationCountry(Country.KG)
                .properties(null)
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createSubstationEntity1, createSubstationEntity2, createSubstationEntity3));
        assertRequestsCount(1, 4, 2, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getSubstationCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createSubstationEntity1.toSubstationCreationInfos());
        assertThat(getSubstationCreationModification(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(createSubstationEntity2.toSubstationCreationInfos());
        assertThat(getSubstationCreationModification(modificationInfos.get(2).getUuid()))
            .recursivelyEquals(createSubstationEntity3.toSubstationCreationInfos());

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createSubstationEntity2.getId(), createSubstationEntity3.getId()));
        assertRequestsCount(4, 0, 1, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(3, 0, 1, 4);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testVoltageLevelCreation() {
        VoltageLevelCreationEntity createVoltLvlEntity1 = VoltageLevelCreationInfos.builder()
                .equipmentId("idVL1")
                .equipmentName("VLName")
                .substationId("s1")
                .nominalV(379.0)
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("bbs.nw").busbarSectionId2("bbs.ne").build()))
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1));
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modificationInfos.size());

        assertThat(getVoltageLevelCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createVoltLvlEntity1.toVoltageLevelCreationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1.getId()));
        assertRequestsCount(4, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    private VoltageLevelCreationInfos makeAVoltageLevelInfos() {

        VoltageLevelCreationInfos createVoltLvlEntity1 = VoltageLevelCreationInfos.builder()
                .substationId("s1").nominalV(379.0).equipmentId("idVL1").equipmentName("VLName")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("bbs.nw").busbarSectionId2("bbs.ne").build()))
                .build();

        return createVoltLvlEntity1;
    }

    @Test
    public void testStatusLineModification() {
        List<OperatingStatusModificationEntity> entities = List.of(
            OperatingStatusModificationInfos.builder().equipmentId("idLine1").action(OperatingStatusModificationInfos.ActionType.LOCKOUT).build().toEntity(),
            OperatingStatusModificationInfos.builder().equipmentId("idLine2").action(OperatingStatusModificationInfos.ActionType.TRIP).build().toEntity(),
            OperatingStatusModificationInfos.builder().equipmentId("idLine3").action(OperatingStatusModificationInfos.ActionType.SWITCH_ON).build().toEntity(),
            OperatingStatusModificationInfos.builder().equipmentId("idLine4").action(OperatingStatusModificationInfos.ActionType.ENERGISE_END_ONE).build().toEntity(),
            OperatingStatusModificationInfos.builder().equipmentId("idLine5").action(OperatingStatusModificationInfos.ActionType.ENERGISE_END_TWO).build().toEntity()
        );

        networkModificationRepository.saveModifications(TEST_GROUP_ID, entities);
        assertRequestsCount(1, 3, 1, 0);

        List<OperatingStatusModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
            .stream()
            .map(OperatingStatusModificationInfos.class::cast)
            .sorted(Comparator.comparing(OperatingStatusModificationInfos::getEquipmentId))
            .collect(Collectors.toList());
        assertEquals(5, modificationInfos.size());

        assertThat(modificationInfos.get(0))
            .recursivelyEquals((OperatingStatusModificationInfos) entities.get(0).toModificationInfos());
        assertThat(modificationInfos.get(1))
            .recursivelyEquals((OperatingStatusModificationInfos) entities.get(1).toModificationInfos());
        assertThat(modificationInfos.get(2))
            .recursivelyEquals((OperatingStatusModificationInfos) entities.get(2).toModificationInfos());
        assertThat(modificationInfos.get(3))
            .recursivelyEquals((OperatingStatusModificationInfos) entities.get(3).toModificationInfos());
        assertThat(modificationInfos.get(4))
            .recursivelyEquals((OperatingStatusModificationInfos) entities.get(4).toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(7, 0, 0, 3);
    }

    @Test
    public void testLineSplitWithVoltageLevel() {
        LineSplitWithVoltageLevelEntity lineSplitEntity1 = LineSplitWithVoltageLevelInfos.builder()
            .lineToSplitId("lineId0")
            .percent(30.0)
            .mayNewVoltageLevelInfos(null)
            .existingVoltageLevelId("vl1")
            .bbsOrBusId("bbsId")
            .newLine1Id("line1id")
            .newLine1Name("line1Name")
            .newLine2Id("line2Id")
            .newLine2Name("line2Name")
            .build().toEntity();
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos();
        LineSplitWithVoltageLevelEntity lineSplitEntity2 = LineSplitWithVoltageLevelInfos.builder()
            .lineToSplitId("lineId1")
            .percent(30.0)
            .mayNewVoltageLevelInfos(voltageLevelCreationInfos)
            .existingVoltageLevelId(null)
            .bbsOrBusId("bbsId")
            .newLine1Id("line1id")
            .newLine1Name("line1Name")
            .newLine2Id("line2Id")
            .newLine2Name("line2Name")
            .build().toEntity();
        VoltageLevelCreationEntity voltageLevelCreationEntity = new VoltageLevelCreationEntity(voltageLevelCreationInfos);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lineSplitEntity1, voltageLevelCreationEntity, lineSplitEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getLineSplitWithVoltageLevelModification(modificationInfos.get(0).getUuid()))
                .recursivelyEquals(lineSplitEntity1.toModificationInfos());

        assertThat(getLineSplitWithVoltageLevelModification(modificationInfos.get(2).getUuid()))
                .recursivelyEquals(lineSplitEntity2.toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(lineSplitEntity1.getId(),
                voltageLevelCreationEntity.getId(),
                lineSplitEntity2.getId()));
        assertRequestsCount(6, 0, 0, 10);

        modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(0, modificationInfos.size());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testLineAttachToVoltageLevel() {
        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .equipmentId("attachmentLineId")
                .r(50.6)
                .x(25.3)
                .build();
        LineAttachToVoltageLevelEntity lineAttachToEntity1 = LineAttachToVoltageLevelInfos.builder()
                .lineToAttachToId("lineId0")
                .percent(40.0)
                .attachmentPointId("AttachmentPointId")
                .attachmentPointName(null)
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("vl1")
                .bbsOrBusId("bbsId")
                .attachmentLine(attachmentLine)
                .newLine1Id("line1Id")
                .newLine1Name("line1Name")
                .newLine2Id("line2Id")
                .newLine2Name("line2Name")
                .build().toEntity();
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos();
        LineAttachToVoltageLevelEntity lineAttachToEntity2 = LineAttachToVoltageLevelInfos.builder()
                .lineToAttachToId("lineId1")
                .percent(40.0)
                .attachmentPointId("AttachmentPointId")
                .attachmentPointName(null)
                .mayNewVoltageLevelInfos(voltageLevelCreationInfos)
                .existingVoltageLevelId(null)
                .bbsOrBusId("bbsId")
                .attachmentLine(attachmentLine)
                .newLine1Id("line1Id")
                .newLine1Name("line1Name")
                .newLine2Id("line2Id")
                .newLine2Name("line2Name")
                .build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lineAttachToEntity1, lineAttachToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(0).getUuid()))
                .recursivelyEquals(
                        lineAttachToEntity1.toModificationInfos());

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(1).getUuid()))
                .recursivelyEquals(
                        lineAttachToEntity2.toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(lineAttachToEntity1.getId(),
                lineAttachToEntity2.getId()));
        assertRequestsCount(5, 0, 0, 12);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testLinesAttachToSplitLines() {
        LinesAttachToSplitLinesEntity linesAttachToEntity1 = LinesAttachToSplitLinesInfos.builder()
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .attachedLineId("lineId3")
                .voltageLevelId("vl1")
                .bbsBusId("bbsId")
                .replacingLine1Id("line1Id")
                .replacingLine1Name("line1Name")
                .replacingLine2Id("line2Id")
                .replacingLine2Name("line2Name")
                .build().toEntity();
        LinesAttachToSplitLinesEntity linesAttachToEntity2 = LinesAttachToSplitLinesInfos.builder()
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .attachedLineId("lineId6")
                .voltageLevelId("vl2")
                .bbsBusId("bbsId2")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .replacingLine2Id("line4Id")
                .replacingLine2Name("line4Name")
                .build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(linesAttachToEntity1, linesAttachToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getLinesAttachToSplitLinesModification(modificationInfos.get(0).getUuid()))
                .recursivelyEquals(
                        linesAttachToEntity1.toModificationInfos());

        assertThat(getLinesAttachToSplitLinesModification(modificationInfos.get(1).getUuid()))
                .recursivelyEquals(
                        linesAttachToEntity2.toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(linesAttachToEntity1.getId(),
                linesAttachToEntity2.getId()));
        assertRequestsCount(2, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testDeleteStashedModificationList() {
        //create a modification and add it to the repository
        var groovyScriptEntity1 = GroovyScriptInfos.builder().stashed(true).script("script1").build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1));
        //check the modification is in the repository
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modificationInfos.size());

        //delete the modification
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1.getId()));
        //check the modification is not in the repository
        modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(0, modificationInfos.size());
    }

    @Test
    public void testDeleteNonStashedModificationList() {
        //create a modification and add it to the repository
        var groovyScriptEntity1 = GroovyScriptInfos.builder().stashed(false).script("script1").build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1));
        //check the modification is in the repository
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modificationInfos.size());

        //delete the modification
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1.getId()));
        //check the modification is not in the repository
        modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(0, modificationInfos.size());
    }

    @Test
    public void testDeleteAttachingLine() {
        DeleteAttachingLineEntity deleteAttachingLineEntity = DeleteAttachingLineInfos.builder()
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .attachedLineId("lineId3")
                .replacingLine1Id("vl1")
                .replacingLine1Name("line1Name")
                .build().toEntity();

        DeleteAttachingLineEntity deleteAttachingLineEntity2 = DeleteAttachingLineInfos.builder()
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .attachedLineId("lineId6")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(deleteAttachingLineEntity, deleteAttachingLineEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(deleteAttachingLineEntity.getId(),
                deleteAttachingLineEntity2.getId()));
        assertRequestsCount(2, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    @Test
    public void testDeleteVoltageLevelOnLine() {
        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity1 = DeleteVoltageLevelOnLineInfos.builder()
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .replacingLine1Id("line1Id")
                .replacingLine1Name("line1Name")
                .build().toEntity();

        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity2 = DeleteVoltageLevelOnLineInfos.builder()
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(deleteVoltageLevelOnLineToEntity1, deleteVoltageLevelOnLineToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(0).getUuid()))
               .recursivelyEquals(
                        deleteVoltageLevelOnLineToEntity1.toModificationInfos());

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(1).getUuid()))
               .recursivelyEquals(
                        deleteVoltageLevelOnLineToEntity2.toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(deleteVoltageLevelOnLineToEntity1.getId(),
                deleteVoltageLevelOnLineToEntity2.getId()));
        assertRequestsCount(2, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    private <T> void testModificationEmbedded(IAttributeModificationEmbeddable<T> modification, T val) {
        assertEquals(val, modification.getValue());
        assertEquals(OperationType.SET, modification.getOpType());
    }

    @Test
    public void testEmbeddedModificationTypes() {
        testModificationEmbedded(new DoubleModificationEmbedded(new AttributeModification<>(10., OperationType.SET)), 10.);
        testModificationEmbedded(new EnumModificationEmbedded<>(new AttributeModification<>(OperationType.SET, OperationType.SET)), OperationType.SET);
        testModificationEmbedded(new BooleanModificationEmbedded(new AttributeModification<>(true, OperationType.SET)), true);
    }

    @Test
    public void testVoltageInitModification() {
        var voltageInitModificationEntity = VoltageInitModificationInfos.builder()
            .generators(List.of(
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("G1")
                    .targetQ(10.)
                    .build(),
                VoltageInitGeneratorModificationInfos.builder()
                    .generatorId("G2")
                    .targetV(226.)
                    .build()))
            .transformers(List.of(
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("2WT1")
                    .ratioTapChangerPosition(3)
                    .build(),
                VoltageInitTransformerModificationInfos.builder()
                    .transformerId("3WT1")
                    .ratioTapChangerPosition(1)
                    .legSide(ThreeSides.TWO)
                    .build()))
            .staticVarCompensators(List.of(
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("SVC1")
                    .reactivePowerSetpoint(50.)
                    .build(),
                VoltageInitStaticVarCompensatorModificationInfos.builder()
                    .staticVarCompensatorId("SVC2")
                    .voltageSetpoint(374.)
                    .build()))
            .vscConverterStations(List.of(
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("VSC1")
                    .reactivePowerSetpoint(40.)
                    .build(),
                VoltageInitVscConverterStationModificationInfos.builder()
                    .vscConverterStationId("VSC2")
                    .voltageSetpoint(224.)
                    .build()))
            .shuntCompensators(List.of(
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v2shunt")
                    .sectionCount(1)
                    .connect(true)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v5shunt")
                    .sectionCount(0)
                    .connect(false)
                    .build(),
                VoltageInitShuntCompensatorModificationInfos.builder()
                    .shuntCompensatorId("v6shunt")
                    .sectionCount(1)
                    .connect(false)
                    .build()))
            .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(voltageInitModificationEntity));
        assertRequestsCount(1, 8, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());

        assertThat(getVoltageInitModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(voltageInitModificationEntity.toModificationInfos());

        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(voltageInitModificationEntity.getId()));
        assertRequestsCount(2, 0, 0, 7);

        SQLStatementCountValidator.reset();
        assertEquals(0, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);
    }

    @Test
    public void testGetModificationCount() {
        var modifEntity1 = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        var modifEntity2 = EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build().toEntity();
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));
        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModificationsCount(TEST_GROUP_ID, false).intValue());
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        assertEquals(0, networkModificationRepository.getModificationsCount(TEST_GROUP_ID, true).intValue());
        assertRequestsCount(1, 0, 0, 0);

        assertThrows(NullPointerException.class, () -> networkModificationRepository.getModificationsCount(null, true));
    }
}
