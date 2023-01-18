/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteAttachingLineEntity;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteVoltageLevelOnLineEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineSplitWithVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LinesAttachToSplitLinesEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherDeleteVoltageLevelOnLineInfos.createMatcherDeleteVoltageLevelOnLineInfos;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@SpringBootTest
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

    @MockBean
    private EquipmentInfosService equipmentInfosService;

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

    private TwoWindingsTransformerCreationInfos getTwoWindingsTransformerCreationModification(UUID modificationUuid) {
        return (TwoWindingsTransformerCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
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

    private DeleteAttachingLineInfos getDeleteAttachingLineModification(UUID modificationUuid) {
        return (DeleteAttachingLineInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private DeleteVoltageLevelOnLineInfos getDeleteVoltageLevelOnLineModification(UUID modificationUuid) {
        return (DeleteVoltageLevelOnLineInfos) networkModificationRepository.getModificationInfo(modificationUuid);
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
        assertThat(getEquipmentAttributeModification(modificationEntities.get(0).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(nullModifEntity.toModificationInfos()));
        assertThat(getEquipmentAttributeModification(modificationEntities.get(1).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(stringModifEntity.toModificationInfos()));
        assertThat(getEquipmentAttributeModification(modificationEntities.get(2).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(boolModifEntity.toModificationInfos()));
        assertThat(getEquipmentAttributeModification(modificationEntities.get(3).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(intModifEntity.toModificationInfos()));
        assertThat(getEquipmentAttributeModification(modificationEntities.get(4).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(floatModifEntity.toModificationInfos()));
        assertThat(getEquipmentAttributeModification(modificationEntities.get(5).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(doubleModifEntity.toModificationInfos()));
        assertThat(getEquipmentAttributeModification(modificationEntities.get(6).getUuid()),
            MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(enumModifEntity.toModificationInfos()));

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

        assertThat(getLoadCreationModification(modificationInfos.get(0).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(createLoadEntity1.toModificationInfos()));
        assertThat(getLoadCreationModification(modificationInfos.get(1).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(createLoadEntity2.toModificationInfos()));
        assertThat(getLoadCreationModification(modificationInfos.get(2).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(createLoadEntity3.toModificationInfos()));

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
        var createGeneratorEntity1 = GeneratorCreationInfos.builder().type(ModificationType.GENERATOR_CREATION)
                .equipmentId("idGenerator1").equipmentName("nameGenerator1")
                .energySource(EnergySource.HYDRO).voltageLevelId("vlId1")
                .busOrBusbarSectionId("busId1").minActivePower(100.0)
                .maxActivePower(800.0).ratedNominalPower(10.)
                .activePowerSetpoint(500).reactivePowerSetpoint(50.)
                .voltageRegulationOn(true).voltageSetpoint(225.).marginalCost(20.)
                .minimumReactivePower(30.).maximumReactivePower(50.)
                .participate(true).droop(8f).transientReactance(37.)
                .stepUpTransformerReactance(46.).regulatingTerminalId("testTerminalId1")
                .regulatingTerminalType("LINE").regulatingTerminalVlId("idVlTest1")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(1).build().toEntity();
        var createGeneratorEntity2 = GeneratorCreationInfos.builder().type(ModificationType.GENERATOR_CREATION)
                .equipmentId("idGenerator2").equipmentName("nameGenerator2")
                .energySource(EnergySource.SOLAR).voltageLevelId("vlId2")
                .busOrBusbarSectionId("busId2").minActivePower(0.0)
                .maxActivePower(300.0).ratedNominalPower(5.)
                .activePowerSetpoint(150).reactivePowerSetpoint(30.)
                .voltageRegulationOn(false).voltageSetpoint(380.).marginalCost(30.)
                .participate(false).droop(null).transientReactance(37.)
                .stepUpTransformerReactance(46.).regulatingTerminalId(null)
                .regulatingTerminalType(null).regulatingTerminalVlId("idVlTest2")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Bot").connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition(2).build().toEntity();

        var createGeneratorEntity3 = GeneratorCreationInfos.builder().type(ModificationType.GENERATOR_CREATION)
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

        assertThat(getGeneratorCreationModification(modificationInfos.get(0).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity1).toModificationInfos()));
        assertThat(getGeneratorCreationModification(modificationInfos.get(1).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity2).toModificationInfos()));
        assertThat(getGeneratorCreationModification(modificationInfos.get(2).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity3).toModificationInfos()));

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
            .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
            .equipmentId("shunt1").equipmentName("nameOne")
            .currentNumberOfSections(1).maximumNumberOfSections(2)
            .susceptancePerSection(1.).isIdenticalSection(true)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();
        var shunt2 = ShuntCompensatorCreationInfos.builder()
            .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
            .equipmentId("shunt2").equipmentName("notNameOne")
            .currentNumberOfSections(1).maximumNumberOfSections(2)
            .susceptancePerSection(1.).isIdenticalSection(true)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();

        var createShuntCompensatorEntity1 = shunt1.toEntity();
        var createShuntCompensatorEntity2 = shunt2.toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createShuntCompensatorEntity1, createShuntCompensatorEntity2));
        assertRequestsCount(1, 5, 2, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getShuntCompensatorCreationModification(modificationInfos.get(0).getUuid()),
            MatcherShuntCompensatorCreationInfos.createMatcher(createShuntCompensatorEntity1.toModificationInfos()));
        assertThat(getShuntCompensatorCreationModification(modificationInfos.get(1).getUuid()),
            MatcherShuntCompensatorCreationInfos.createMatcher(createShuntCompensatorEntity2.toModificationInfos()));

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
        var createLineEntity1 = LineCreationInfos.builder().type(ModificationType.LINE_CREATION).equipmentId("idLine1").equipmentName("nameLine1").seriesResistance(1.0).seriesReactance(1.1).shuntConductance1(10.0).shuntSusceptance1(11.0).shuntConductance2(100.0).shuntSusceptance2(100.1).voltageLevelId1("vlId11").busOrBusbarSectionId1("busId11").voltageLevelId2("vlId12").busOrBusbarSectionId2("busId12").connectionName1("cn11").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn22").connectionDirection2(ConnectablePosition.Direction.TOP).build().toEntity();
        var createLineEntity2 = LineCreationInfos.builder().type(ModificationType.LINE_CREATION).equipmentId("idLine2").equipmentName("nameLine2").seriesResistance(2.0).seriesReactance(2.2).shuntConductance1(20.0).shuntSusceptance1(22.0).shuntConductance2(200.0).shuntSusceptance2(200.2).voltageLevelId1("vlId21").busOrBusbarSectionId1("busId21").voltageLevelId2("vlId22").busOrBusbarSectionId2("busId22").connectionName1("cn33").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn44").connectionDirection2(ConnectablePosition.Direction.BOTTOM).currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.0).build()).build().toEntity();
        var createLineEntity3 = LineCreationInfos.builder().type(ModificationType.LINE_CREATION).equipmentId("idLine3").equipmentName("nameLine3").seriesResistance(3.0).seriesReactance(3.3).shuntConductance1(30.0).shuntSusceptance1(33.0).shuntConductance2(300.0).shuntSusceptance2(300.3).voltageLevelId1("vlId31").busOrBusbarSectionId1("busId31").voltageLevelId2("vlId32").busOrBusbarSectionId2("busId32").connectionName1("cn55").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn66").connectionDirection2(ConnectablePosition.Direction.TOP).currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).build()).build().toEntity();
        var createLineEntity4 = LineCreationInfos.builder().type(ModificationType.LINE_CREATION).equipmentId("idLine4").equipmentName("nameLine4").seriesResistance(3.0).seriesReactance(3.3).shuntConductance1(null).shuntSusceptance1(null).shuntConductance2(null).shuntSusceptance2(null).voltageLevelId1("vlId41").busOrBusbarSectionId1("busId41").voltageLevelId2("vlId42").busOrBusbarSectionId2("busId42").connectionName1("cn77").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn88").connectionDirection2(ConnectablePosition.Direction.BOTTOM).currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).build()).currentLimits2(CurrentLimitsInfos.builder().permanentLimit(4.0).build()).build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLineEntity1, createLineEntity2, createLineEntity3, createLineEntity4));
        assertRequestsCount(1, 13, 4, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(4, modificationInfos.size());

        assertThat(getLineCreationModification(modificationInfos.get(0).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity1).toModificationInfos()));
        assertThat(getLineCreationModification(modificationInfos.get(1).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity2).toModificationInfos()));
        assertThat(getLineCreationModification(modificationInfos.get(2).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity3).toModificationInfos()));
        assertThat(getLineCreationModification(modificationInfos.get(3).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity4).toModificationInfos()));

        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createLineEntity2.getId(), createLineEntity3.getId()));
        assertRequestsCount(4, 0, 2, 6);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(4, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(4, 0, 0, 7);

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
        assertRequestsCount(4, 1, 6, 0);

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
        assertEquals(List.of(groovyScriptEntity3.getId()), networkModificationRepository.moveModifications(TEST_GROUP_ID_3, TEST_GROUP_ID, modificationsToMoveUuid, null).getModificationsInError());
        assertRequestsCount(4, 0, 5, 0);

        // moving modification with reference node not in destination: no exception, and bad id is returned as error
        SQLStatementCountValidator.reset();
        List <UUID> modificationsToMoveUuid2 = List.of(groovyScriptEntity1.getId());
        UUID referenceNodeUuid = groovyScriptEntity2.getId();
        NetworkModificationRepository.MoveModificationResult result = networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, modificationsToMoveUuid2, referenceNodeUuid);
        assertTrue(result.getModificationsMoved().isEmpty()); // nothing moved
        assertEquals(modificationsToMoveUuid2, result.getModificationsInError());
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

        assertThat(getGroovyScript(modificationInfos.get(0).getUuid()),
            MatcherGroovyScriptInfos.createMatcherGroovyScriptInfos(groovyScriptEntity1.toModificationInfos()));
        assertThat(getGroovyScript(modificationInfos.get(1).getUuid()),
            MatcherGroovyScriptInfos.createMatcherGroovyScriptInfos(groovyScriptEntity2.toModificationInfos()));
        assertThat(getGroovyScript(modificationInfos.get(2).getUuid()),
            MatcherGroovyScriptInfos.createMatcherGroovyScriptInfos(groovyScriptEntity3.toModificationInfos()));

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
                .type(ModificationType.SUBSTATION_CREATION)
                .equipmentId("idSubstation1")
                .equipmentName("nameSubstation1")
                .substationCountry(Country.FR)
                .properties(null)
                .build().toEntity();
        var createSubstationEntity2 = SubstationCreationInfos.builder()
                .type(ModificationType.SUBSTATION_CREATION)
                .equipmentId("idSubstation2")
                .equipmentName("nameSubstation2")
                .substationCountry(Country.TD)
                .properties(null)
                .build().toEntity();
        var createSubstationEntity3 = SubstationCreationInfos.builder()
                .type(ModificationType.SUBSTATION_CREATION)
                .equipmentId("idSubstation3")
                .equipmentName("nameSubstation3")
                .substationCountry(Country.KG)
                .properties(null)
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createSubstationEntity1, createSubstationEntity2, createSubstationEntity3));
        assertRequestsCount(1, 7, 3, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getSubstationCreationModification(modificationInfos.get(0).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity1).toSubstationCreationInfos()));
        assertThat(getSubstationCreationModification(modificationInfos.get(1).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity2).toSubstationCreationInfos()));
        assertThat(getSubstationCreationModification(modificationInfos.get(2).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity3).toSubstationCreationInfos()));

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
        List<BusbarSectionCreationInfos> bbses = new ArrayList<>();
        Stream.iterate(1, n -> n + 1).limit(3 + 1).forEach(i -> bbses.add(new BusbarSectionCreationInfos("bbs" + i, "NW", 1 + i, 1)));

        List<BusbarConnectionCreationInfos> cnxes = new ArrayList<>();
        Stream.iterate(0, n -> n + 1).limit(3).forEach(i -> {
            cnxes.add(new BusbarConnectionCreationInfos("bbs.nw", "bbs.ne", SwitchKind.BREAKER));
            cnxes.add(new BusbarConnectionCreationInfos("bbs.nw", "bbs.ne", SwitchKind.DISCONNECTOR));
        });

        VoltageLevelCreationEntity createVoltLvlEntity1 = VoltageLevelCreationInfos.builder().type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .equipmentId("idVL1")
                .equipmentName("VLName")
                .nominalVoltage(379.0)
                .substationId("s1")
                .busbarSections(bbses)
                .busbarConnections(cnxes)
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1));
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modificationInfos.size());

        assertThat(getVoltageLevelCreationModification(modificationInfos.get(0).getUuid()),
            MatcherVoltageLevelCreationInfos.createMatcherVoltageLevelCreationInfos(createVoltLvlEntity1.toVoltageLevelCreationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true)
        );
    }

    private VoltageLevelCreationInfos makeAVoltageLevelInfos(int nbBBSs, int nbCnxs) {
        List<BusbarSectionCreationInfos> bbses;
        if (nbBBSs < 0) {
            bbses = null;
        } else {
            bbses = new ArrayList<>();
            Stream.iterate(1, n -> n + 1).limit(nbBBSs + 1).forEach(i -> bbses.add(new BusbarSectionCreationInfos("bbs" + i, "NW", 1 + i, 1)));
        }

        List<BusbarConnectionCreationInfos> cnxes;
        if (nbCnxs < 0) {
            cnxes = null;
        } else {
            cnxes = new ArrayList<>();
            Stream.iterate(0, n -> n + 1).limit(nbBBSs).forEach(i -> {
                cnxes.add(new BusbarConnectionCreationInfos("bbs.nw", "bbs.ne", SwitchKind.BREAKER));
                cnxes.add(new BusbarConnectionCreationInfos("bbs.nw", "bbs.ne", SwitchKind.DISCONNECTOR));
            });
        }

        VoltageLevelCreationInfos createVoltLvlEntity1 = VoltageLevelCreationInfos.builder().type(ModificationType.VOLTAGE_LEVEL_CREATION)
            .substationId("s1").nominalVoltage(379.0).equipmentId("idVL1").equipmentName("VLName")
            .busbarSections(bbses).busbarConnections(cnxes)
            .build();

        return createVoltLvlEntity1;
    }

    @Test
    public void testStatusLineModification() {
        List<BranchStatusModificationEntity> entities = List.of(
            BranchStatusModificationInfos.builder().type(ModificationType.BRANCH_STATUS_MODIFICATION).equipmentId("idLine1").action(BranchStatusModificationInfos.ActionType.LOCKOUT).build().toEntity(),
            BranchStatusModificationInfos.builder().type(ModificationType.BRANCH_STATUS_MODIFICATION).equipmentId("idLine2").action(BranchStatusModificationInfos.ActionType.TRIP).build().toEntity(),
            BranchStatusModificationInfos.builder().type(ModificationType.BRANCH_STATUS_MODIFICATION).equipmentId("idLine3").action(BranchStatusModificationInfos.ActionType.SWITCH_ON).build().toEntity(),
            BranchStatusModificationInfos.builder().type(ModificationType.BRANCH_STATUS_MODIFICATION).equipmentId("idLine4").action(BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE).build().toEntity(),
            BranchStatusModificationInfos.builder().type(ModificationType.BRANCH_STATUS_MODIFICATION).equipmentId("idLine5").action(BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO).build().toEntity()
        );

        networkModificationRepository.saveModifications(TEST_GROUP_ID, entities);
        assertRequestsCount(1, 11, 5, 0);

        List<BranchStatusModificationInfos> modificationInfos = networkModificationRepository.getModifications(
                entities.stream().map(ModificationEntity::getId).collect(Collectors.toList())
            )
            .stream()
            .map(BranchStatusModificationInfos.class::cast)
            .sorted(Comparator.comparing(BranchStatusModificationInfos::getEquipmentId))
            .collect(Collectors.toList());
        assertEquals(5, modificationInfos.size());

        assertThat(modificationInfos.get(0),
            MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) entities.get(0).toModificationInfos()));
        assertThat(modificationInfos.get(1),
            MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) entities.get(1).toModificationInfos()));
        assertThat(modificationInfos.get(2),
            MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) entities.get(2).toModificationInfos()));
        assertThat(modificationInfos.get(3),
            MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) entities.get(3).toModificationInfos()));
        assertThat(modificationInfos.get(4),
            MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos((BranchStatusModificationInfos) entities.get(4).toModificationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 11);
    }

    @Test
    public void testLineSplitWithVoltageLevel() {
        LineSplitWithVoltageLevelEntity lineSplitEntity1 = LineSplitWithVoltageLevelInfos.builder().type(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL)
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
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos(1, 0);
        LineSplitWithVoltageLevelEntity lineSplitEntity2 = LineSplitWithVoltageLevelInfos.builder().type(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL)
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

        assertThat(getLineSplitWithVoltageLevelModification(modificationInfos.get(0).getUuid()),
                MatcherLineSplitWithVoltageLevelInfos.createMatcherLineSplitWithVoltageLevelInfos(
                        lineSplitEntity1.toModificationInfos()));

        assertThat(getLineSplitWithVoltageLevelModification(modificationInfos.get(2).getUuid()),
                MatcherLineSplitWithVoltageLevelInfos.createMatcherLineSplitWithVoltageLevelInfos(
                        lineSplitEntity2.toModificationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(lineSplitEntity1.getId(),
                voltageLevelCreationEntity.getId(),
                lineSplitEntity2.getId()));
        assertRequestsCount(3, 0, 0, 12);

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
                .type(ModificationType.LINE_CREATION)
                .equipmentId("attachmentLineId")
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();
        LineAttachToVoltageLevelEntity lineAttachToEntity1 = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
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
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos(1, 0);
        LineAttachToVoltageLevelEntity lineAttachToEntity2 = LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
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

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(0).getUuid()),
                MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos(
                        lineAttachToEntity1.toModificationInfos()));

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(1).getUuid()),
                MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos(
                        lineAttachToEntity2.toModificationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(lineAttachToEntity1.getId(),
                lineAttachToEntity2.getId()));
        assertRequestsCount(2, 0, 0, 12);

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
                .type(ModificationType.LINES_ATTACH_TO_SPLIT_LINES)
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
                .type(ModificationType.LINES_ATTACH_TO_SPLIT_LINES)
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

        assertThat(getLinesAttachToSplitLinesModification(modificationInfos.get(0).getUuid()),
                MatcherLinesAttachToSplitLinesInfos.createMatcherLinesAttachToSplitLinesInfos(
                        linesAttachToEntity1.toModificationInfos()));

        assertThat(getLinesAttachToSplitLinesModification(modificationInfos.get(1).getUuid()),
                MatcherLinesAttachToSplitLinesInfos.createMatcherLinesAttachToSplitLinesInfos(
                        linesAttachToEntity2.toModificationInfos()));

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
        DeleteAttachingLineEntity deleteAttachingLineEntity = DeleteAttachingLineInfos.builder().type(ModificationType.DELETE_ATTACHING_LINE)
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .attachedLineId("lineId3")
                .replacingLine1Id("vl1")
                .replacingLine1Name("line1Name")
                .build().toEntity();

        DeleteAttachingLineEntity deleteAttachingLineEntity2 = DeleteAttachingLineInfos.builder().type(ModificationType.DELETE_ATTACHING_LINE)
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
        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity1 = DeleteVoltageLevelOnLineInfos.builder().type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .replacingLine1Id("line1Id")
                .replacingLine1Name("line1Name")
                .build().toEntity();

        DeleteVoltageLevelOnLineEntity deleteVoltageLevelOnLineToEntity2 = DeleteVoltageLevelOnLineInfos.builder().type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .build().toEntity();

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(deleteVoltageLevelOnLineToEntity1, deleteVoltageLevelOnLineToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(0).getUuid()),
                createMatcherDeleteVoltageLevelOnLineInfos(
                        deleteVoltageLevelOnLineToEntity1.toModificationInfos()));

        assertThat(getDeleteVoltageLevelOnLineModification(modificationInfos.get(1).getUuid()),
                createMatcherDeleteVoltageLevelOnLineInfos(
                        deleteVoltageLevelOnLineToEntity2.toModificationInfos()));

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
        EqualsVerifier.simple().forClass(modification.getClass()).verify();
    }

    @Test
    public void testEmbeddedModificationTypes() {
        testModificationEmbedded(new DoubleModificationEmbedded(new AttributeModification<>(10., OperationType.SET)), 10.);
        testModificationEmbedded(new EnumModificationEmbedded<>(new AttributeModification<>(OperationType.SET, OperationType.SET)), OperationType.SET);
        testModificationEmbedded(new BooleanModificationEmbedded(new AttributeModification<>(true, OperationType.SET)), true);
    }
}
