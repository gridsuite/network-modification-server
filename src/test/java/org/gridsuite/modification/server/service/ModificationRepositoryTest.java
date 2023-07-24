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

    private TableEquipmentModificationInfos getTableEquipmentModification(UUID modificationUuid) {
        return (TableEquipmentModificationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
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

        assertRequestsCount(1, 5, 2, 0);
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
        assertRequestsCount(2, 0, 1, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 3);

        // Non-existent group modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testLoadCreation() {
        var createLoadEntity1 = LoadCreationInfos.builder().equipmentId("idLoad1").equipmentName("nameLoad1").loadType(LoadType.AUXILIARY).voltageLevelId("vlId1").busOrBusbarSectionId("busId1").activePower(100.).reactivePower(20.).connectionName("top1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(11).build().toEntity();
        var createLoadEntity2 = LoadCreationInfos.builder().equipmentId("idLoad2").equipmentName("nameLoad2").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId2").busOrBusbarSectionId("busId2").activePower(80.).reactivePower(30.).connectionName("bottom1").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(11).build().toEntity();
        var createLoadEntity3 = LoadCreationInfos.builder().equipmentId("idLoad3").equipmentName("nameLoad3").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId3").busOrBusbarSectionId("busId3").activePower(50.).reactivePower(90.).connectionName("top2").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(12).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLoadEntity1, createLoadEntity2, createLoadEntity3));
        assertRequestsCount(1, 7, 3, 0);

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
        assertRequestsCount(2, 0, 1, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testGeneratorCreation() {
        var createGeneratorEntity1 = GeneratorCreationInfos.builder()
                .equipmentId("idGenerator1").equipmentName("nameGenerator1")
                .energySource(EnergySource.HYDRO).voltageLevelId("vlId1")
                .busOrBusbarSectionId("busId1").minActivePower(100.0)
                .maxActivePower(800.0).ratedNominalPower(10.)
                .activePowerSetpoint(500).reactivePowerSetpoint(50.)
                .voltageRegulationOn(true).voltageSetpoint(225.)
                .plannedActivePowerSetPoint(20.)
                .marginalCost(20.)
                .plannedOutageRate(20.).forcedOutageRate(20.)
                .minimumReactivePower(30.).maximumReactivePower(50.)
                .participate(true).droop(8f).transientReactance(37.)
                .stepUpTransformerReactance(46.).regulatingTerminalId("testTerminalId1")
                .regulatingTerminalType("LINE").regulatingTerminalVlId("idVlTest1")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(1).build().toEntity();
        var createGeneratorEntity2 = GeneratorCreationInfos.builder()
                .equipmentId("idGenerator2").equipmentName("nameGenerator2")
                .energySource(EnergySource.SOLAR).voltageLevelId("vlId2")
                .busOrBusbarSectionId("busId2").minActivePower(0.0)
                .maxActivePower(300.0).ratedNominalPower(5.)
                .activePowerSetpoint(150).reactivePowerSetpoint(30.)
                .voltageRegulationOn(false).voltageSetpoint(380.)
                .plannedActivePowerSetPoint(30.)
                .marginalCost(30.)
                .plannedOutageRate(30.).forcedOutageRate(30.)
                .participate(false).droop(null).transientReactance(37.)
                .stepUpTransformerReactance(46.).regulatingTerminalId(null)
                .regulatingTerminalType(null).regulatingTerminalVlId("idVlTest2")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Bot").connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition(2).build().toEntity();

        var createGeneratorEntity3 = GeneratorCreationInfos.builder()
                .equipmentId("idGenerator3").equipmentName("nameGenerator3")
                .energySource(EnergySource.OTHER).voltageLevelId("vlId3")
                .busOrBusbarSectionId("busId3").minActivePower(10.0)
                .maxActivePower(900.0).ratedNominalPower(20.)
                .voltageRegulationOn(true).voltageSetpoint(150.).marginalCost(null)
                .participate(false).droop(null).transientReactance(null)
                .stepUpTransformerReactance(null).regulatingTerminalId("testTerminalId2")
                .regulatingTerminalType("BATTERY").regulatingTerminalVlId("idVlTest2")
                .qPercent(25.).reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(new ReactiveCapabilityCurveCreationInfos(33., 44., 55.)))
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(3).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createGeneratorEntity1, createGeneratorEntity2, createGeneratorEntity3));
        assertRequestsCount(1, 8, 3, 0);

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
        assertRequestsCount(2, 0, 1, 6);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 4);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testShuntCompensatorCreation() {
        var shunt1 = ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt1").equipmentName("nameOne")
            .maximumNumberOfSections(2)
            .susceptancePerSection(1.)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();
        var shunt2 = ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt2").equipmentName("notNameOne")
            .maximumNumberOfSections(2)
            .susceptancePerSection(1.)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();

        var createShuntCompensatorEntity1 = shunt1.toEntity();
        var createShuntCompensatorEntity2 = shunt2.toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createShuntCompensatorEntity1, createShuntCompensatorEntity2));
        assertRequestsCount(1, 5, 2, 0);

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
        assertRequestsCount(2, 0, 1, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void testLineCreation() {
        var createLineEntity1 = LineCreationInfos.builder().equipmentId("idLine1").equipmentName("nameLine1").seriesResistance(1.0).seriesReactance(1.1).shuntConductance1(10.0).shuntSusceptance1(11.0).shuntConductance2(100.0).shuntSusceptance2(100.1).voltageLevelId1("vlId11").busOrBusbarSectionId1("busId11").voltageLevelId2("vlId12").busOrBusbarSectionId2("busId12").connectionName1("cn11").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn22").connectionDirection2(ConnectablePosition.Direction.TOP).build().toEntity();
        var createLineEntity2 = LineCreationInfos.builder().equipmentId("idLine2").equipmentName("nameLine2").seriesResistance(2.0).seriesReactance(2.2).shuntConductance1(20.0).shuntSusceptance1(22.0).shuntConductance2(200.0).shuntSusceptance2(200.2).voltageLevelId1("vlId21").busOrBusbarSectionId1("busId21").voltageLevelId2("vlId22").busOrBusbarSectionId2("busId22").connectionName1("cn33").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn44").connectionDirection2(ConnectablePosition.Direction.BOTTOM).currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build()).build().toEntity();
        var createLineEntity3 = LineCreationInfos.builder().equipmentId("idLine3").equipmentName("nameLine3").seriesResistance(3.0).seriesReactance(3.3).shuntConductance1(30.0).shuntSusceptance1(33.0).shuntConductance2(300.0).shuntSusceptance2(300.3).voltageLevelId1("vlId31").busOrBusbarSectionId1("busId31").voltageLevelId2("vlId32").busOrBusbarSectionId2("busId32").connectionName1("cn55").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn66").connectionDirection2(ConnectablePosition.Direction.TOP).currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build()).build().toEntity();
        var createLineEntity4 = LineCreationInfos.builder().equipmentId("idLine4").equipmentName("nameLine4").seriesResistance(3.0).seriesReactance(3.3).shuntConductance1(null).shuntSusceptance1(null).shuntConductance2(null).shuntSusceptance2(null).voltageLevelId1("vlId41").busOrBusbarSectionId1("busId41").voltageLevelId2("vlId42").busOrBusbarSectionId2("busId42").connectionName1("cn77").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn88").connectionDirection2(ConnectablePosition.Direction.BOTTOM).currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build()).currentLimits2(CurrentLimitsInfos.builder().permanentLimit(4.0).temporaryLimits(Collections.emptyList()).build()).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLineEntity1, createLineEntity2, createLineEntity3, createLineEntity4));
        assertRequestsCount(1, 13, 4, 0);

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
        assertRequestsCount(4, 0, 2, 8);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(6, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(4, 0, 0, 9);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void insertModificationTest() {
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build().toEntity();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build().toEntity();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build().toEntity();
        var groovyScriptEntity4 = GroovyScriptInfos.builder().script("script4").build().toEntity();
        var groovyScriptEntity5 = GroovyScriptInfos.builder().script("script5").build().toEntity();
        var groovyScriptEntity6 = GroovyScriptInfos.builder().script("scriptSaucisse").build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2,
            groovyScriptEntity3, groovyScriptEntity4, groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 13, 6, 0);

        var modificationOriginal = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, TEST_GROUP_ID, List.of(groovyScriptEntity6.getId()), groovyScriptEntity2.getId());
        assertRequestsCount(2, 0, 6, 0);

        var modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        // [0:1, 1:6, 2:2, 3:3, 4:4 ,5:5 ]
        var expected = List.of(modificationOriginal.get(0), modificationOriginal.get(5),
            modificationOriginal.get(1), modificationOriginal.get(2), modificationOriginal.get(3), modificationOriginal.get(4));

        assertEquals(getIds(expected), getIds(modification));

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, TEST_GROUP_ID, List.of(groovyScriptEntity3.getId(), groovyScriptEntity6.getId()), null);
        assertRequestsCount(2, 0, 6, 0);

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
        assertRequestsCount(1, 9, 4, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 5, 2, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId()), null);
        assertRequestsCount(4, 0, 8, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        var expected1 = List.of(modificationOriginal1.get(0), modificationOriginal1.get(3));
        var expected2 = List.of(modificationOriginal2.get(0), modificationOriginal2.get(1), modificationOriginal1.get(1), modificationOriginal1.get(2));

        assertEquals(getIds(expected1), getIds(modification1));
        assertEquals(getIds(expected2), getIds(modification2));

        // cutting and pasting to non existing group should work
        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID_3, TEST_GROUP_ID_2, List.of(expected2.get(0).getUuid(), expected2.get(1).getUuid()), null);
        assertRequestsCount(3, 1, 6, 0);

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
        assertRequestsCount(1, 9, 4, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 5, 2, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId()), groovyScriptEntity6.getId());
        assertRequestsCount(4, 0, 8, 0);

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
        assertRequestsCount(1, 5, 2, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(1, 5, 2, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(1, 5, 2, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modificationOriginal3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        // moving modifications from a wrong group should work but return their UUID in response
        SQLStatementCountValidator.reset();
        List<UUID> modificationsToMoveUuid = List.of(groovyScriptEntity1.getId(), groovyScriptEntity3.getId());
        networkModificationRepository.moveModifications(TEST_GROUP_ID_3, TEST_GROUP_ID, modificationsToMoveUuid, null);
        assertRequestsCount(4, 0, 5, 0);

        // moving modification with reference node not in destination: no exception, and bad id is returned as error
        SQLStatementCountValidator.reset();
        List <UUID> modificationsToMoveUuid2 = List.of(groovyScriptEntity1.getId());
        UUID referenceNodeUuid = groovyScriptEntity2.getId();
        List<ModificationEntity> result = networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, modificationsToMoveUuid2, referenceNodeUuid);
        assertTrue(result.isEmpty()); // nothing moved
        assertRequestsCount(2, 0, 0, 0);

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
        assertRequestsCount(1, 7, 3, 0);

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
        assertRequestsCount(2, 0, 1, 4);

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
                .properties(null)
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
        assertRequestsCount(1, 7, 3, 0);

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
        assertRequestsCount(5, 0, 1, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(3, 0, 0, 3);

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
                .nominalVoltage(379.0)
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
        assertRequestsCount(3, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    private VoltageLevelCreationInfos makeAVoltageLevelInfos() {

        VoltageLevelCreationInfos createVoltLvlEntity1 = VoltageLevelCreationInfos.builder()
                .substationId("s1").nominalVoltage(379.0).equipmentId("idVL1").equipmentName("VLName")
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
        List<BranchStatusModificationEntity> entities = List.of(
            BranchStatusModificationInfos.builder().equipmentId("idLine1").action(BranchStatusModificationInfos.ActionType.LOCKOUT).build().toEntity(),
            BranchStatusModificationInfos.builder().equipmentId("idLine2").action(BranchStatusModificationInfos.ActionType.TRIP).build().toEntity(),
            BranchStatusModificationInfos.builder().equipmentId("idLine3").action(BranchStatusModificationInfos.ActionType.SWITCH_ON).build().toEntity(),
            BranchStatusModificationInfos.builder().equipmentId("idLine4").action(BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE).build().toEntity(),
            BranchStatusModificationInfos.builder().equipmentId("idLine5").action(BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO).build().toEntity()
        );

        networkModificationRepository.saveModifications(TEST_GROUP_ID, entities);
        assertRequestsCount(1, 11, 5, 0);

        List<BranchStatusModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
            .stream()
            .map(BranchStatusModificationInfos.class::cast)
            .sorted(Comparator.comparing(BranchStatusModificationInfos::getEquipmentId))
            .collect(Collectors.toList());
        assertEquals(5, modificationInfos.size());

        assertThat(modificationInfos.get(0))
            .recursivelyEquals((BranchStatusModificationInfos) entities.get(0).toModificationInfos());
        assertThat(modificationInfos.get(1))
            .recursivelyEquals((BranchStatusModificationInfos) entities.get(1).toModificationInfos());
        assertThat(modificationInfos.get(2))
            .recursivelyEquals((BranchStatusModificationInfos) entities.get(2).toModificationInfos());
        assertThat(modificationInfos.get(3))
            .recursivelyEquals((BranchStatusModificationInfos) entities.get(3).toModificationInfos());
        assertThat(modificationInfos.get(4))
            .recursivelyEquals((BranchStatusModificationInfos) entities.get(4).toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 11);
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
        assertRequestsCount(4, 0, 0, 12);

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
                .seriesResistance(50.6)
                .seriesReactance(25.3)
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
        assertRequestsCount(3, 0, 0, 12);

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
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
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
        assertRequestsCount(2, 0, 0, 4);

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
        assertRequestsCount(2, 0, 0, 4);

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
    public void testTableEquipmentModification() {
        var tableEquipmentModificationEntity = TableEquipmentModificationInfos.builder()
            .modifications(List.of(
                GeneratorModificationInfos.builder()
                    .equipmentId("G1")
                    .reactivePowerSetpoint(new AttributeModification<>(10., OperationType.SET))
                    .build(),
                GeneratorModificationInfos.builder()
                    .equipmentId("G2")
                    .voltageSetpoint(new AttributeModification<>(226., OperationType.SET))
                    .build()))
            .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(tableEquipmentModificationEntity));
        assertRequestsCount(1, 9, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());

        assertThat(getTableEquipmentModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(tableEquipmentModificationEntity.toModificationInfos());

        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(tableEquipmentModificationEntity.getId()));
        assertRequestsCount(3, 0, 0, 9);

        SQLStatementCountValidator.reset();
        assertEquals(0, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);
    }
}
