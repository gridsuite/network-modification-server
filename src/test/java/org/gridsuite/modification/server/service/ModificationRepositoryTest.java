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
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.server.dto.CompositeInfos;
import org.gridsuite.modification.server.dto.ModificationApplicability;
import org.gridsuite.modification.server.dto.ModificationContainerInfos;
import org.gridsuite.modification.server.entities.ModificationContainerType;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;
import org.gridsuite.modification.server.error.NetworkModificationServerException;
import org.gridsuite.modification.server.repositories.ModificationContainerRepository;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.powsybl.iidm.network.StaticVarCompensator.RegulationMode.VOLTAGE;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE1;
import static org.gridsuite.modification.dto.OperationalLimitsGroupInfos.Applicability.SIDE2;
import static org.gridsuite.modification.dto.VoltageRegulationType.DISTANT;
import static org.gridsuite.modification.server.error.ModificationBusinessErrorCode.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertRequestsCount;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SpringBootTest
@DisableElasticsearch
@Tag("IntegrationTest")
class ModificationRepositoryTest {
    private static final UUID TEST_GROUP_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID_2 = UUID.fromString("5809dabf-60f8-46e5-9e58-57b03d6b1818");
    private static final UUID TEST_GROUP_ID_3 = UUID.fromString("de67bab1-f47b-4199-80a7-10bd77285675");
    private static final String ROOT_NETWORK_TAG = "PH1";
    private static final String OTHER_ROOT_NETWORK_TAG = "PH2";
    private static final String RENAMED_ROOT_NETWORK_TAG = "PH3";

    @Autowired
    private ModificationGroupRepository modificationGroupRepository;

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    @Autowired
    private ModificationContainerRepository modificationContainerRepository;

    @Autowired
    private ModificationRepository modificationRepository;

    @BeforeEach
    void setUp() {
        networkModificationRepository.deleteAll();
        SQLStatementCountValidator.reset();
    }

    private EquipmentAttributeModificationInfos getEquipmentAttributeModification(UUID modificationUuid) {
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

    private GroovyScriptInfos getGroovyScript(UUID modificationUuid) {
        return (GroovyScriptInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private ShuntCompensatorCreationInfos getShuntCompensatorCreationModification(UUID modificationUuid) {
        return (ShuntCompensatorCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
    }

    private StaticVarCompensatorCreationInfos getStaticVarCompensatorCreationModification(UUID modificationUuid) {
        return (StaticVarCompensatorCreationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
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
    void test() {
        assertEquals(List.of(), this.networkModificationRepository.getModificationGroupsUuids());
        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
                new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
        assertEquals(0, networkModificationRepository.getModifications(TEST_GROUP_ID, true, false).size());

        var nullModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id0").equipmentAttributeName("attribute").equipmentAttributeValue(null).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var stringModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id1").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var boolModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue(true).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var intModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id3").equipmentAttributeName("attribute").equipmentAttributeValue(1).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build());
        var floatModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id4").equipmentAttributeName("attribute").equipmentAttributeValue(2F).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build());
        var doubleModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id5").equipmentAttributeName("attribute").equipmentAttributeValue(3D).equipmentType(IdentifiableType.VOLTAGE_LEVEL).build());
        var enumModifEntity = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id6").equipmentAttributeName("attribute").equipmentAttributeValue(SwitchKind.BREAKER).equipmentType(
                        IdentifiableType.VOLTAGE_LEVEL).build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(nullModifEntity, stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity,
                enumModifEntity));

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
        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testCreateModificationGroupQueryCount() {
        modificationGroupRepository.save(new ModificationGroupEntity(TEST_GROUP_ID));

        // No select
        assertRequestsCount(0, 1, 0, 0);
    }

    @Test
    void testCreateModificationQueryCount() {
        var modifEntity1 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id1").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var modifEntity2 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        assertRequestsCount(2, 3, 0, 0);
    }

    @Test
    void testGetModificationQueryCount() {
        var modifEntity1 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id1").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var modifEntity2 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var modifEntity3 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id3").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2, modifEntity3));

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        getEquipmentAttributeModification(modifEntity1.getId());
        // the modification itself, then the applicabilities of the tree it may hold
        assertRequestsCount(2, 0, 0, 0);

        // Non-existent modification uuid
        assertThrows(NetworkModificationServerException.class, () -> getEquipmentAttributeModification(TEST_GROUP_ID),
                new NetworkModificationServerException(MODIFICATION_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testDeleteModificationQueryCount() {
        var modifEntity1 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var modifEntity2 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(modifEntity1.getId()));
        assertRequestsCount(5, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 3);

        // Non-existent group modification uuid
        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testLoadCreation() {
        var createLoadEntity1 = ModificationEntity.fromDTO(
                LoadCreationInfos.builder().equipmentId("idLoad1").equipmentName("nameLoad1").loadType(LoadType.AUXILIARY).voltageLevelId("vlId1").busOrBusbarSectionId("busId1").p0(100.).q0(
                        20.).connectionName("top1").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(11).build());
        var createLoadEntity2 = ModificationEntity.fromDTO(
                LoadCreationInfos.builder().equipmentId("idLoad2").equipmentName("nameLoad2").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId2").busOrBusbarSectionId("busId2").p0(80.).q0(
                        30.).connectionName("bottom1").connectionDirection(ConnectablePosition.Direction.BOTTOM).connectionPosition(11).build());
        var createLoadEntity3 = ModificationEntity.fromDTO(
                LoadCreationInfos.builder().equipmentId("idLoad3").equipmentName("nameLoad3").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId3").busOrBusbarSectionId("busId3").p0(50.).q0(
                        90.).connectionName("top2").connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(12).build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLoadEntity1, createLoadEntity2, createLoadEntity3));
        assertRequestsCount(2, 3, 0, 0);

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
        assertRequestsCount(6, 0, 0, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 3);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testGeneratorCreation() {
        var createGeneratorEntity1 = ModificationEntity.fromDTO(GeneratorCreationInfos.builder()
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
                .connectionPosition(1).build());
        var createGeneratorEntity2 = ModificationEntity.fromDTO(GeneratorCreationInfos.builder()
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
                .connectionPosition(2).build());

        var createGeneratorEntity3 = ModificationEntity.fromDTO(GeneratorCreationInfos.builder()
                .equipmentId("idGenerator3").equipmentName("nameGenerator3")
                .energySource(EnergySource.OTHER).voltageLevelId("vlId3")
                .busOrBusbarSectionId("busId3").minP(10.0)
                .maxP(900.0).ratedS(20.)
                .voltageRegulationOn(true).targetV(150.).marginalCost(null)
                .participate(false).droop(null).directTransX(null)
                .stepUpTransformerX(null).regulatingTerminalId("testTerminalId2")
                .regulatingTerminalType("BATTERY").regulatingTerminalVlId("idVlTest2")
                .qPercent(25.).reactiveCapabilityCurve(true).reactiveCapabilityCurvePoints(List.of(new ReactiveCapabilityCurvePointsInfos(33., 44., 55.)))
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(3).build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createGeneratorEntity1, createGeneratorEntity2, createGeneratorEntity3));
        assertRequestsCount(2, 4, 0, 0);

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
        assertRequestsCount(6, 0, 0, 3);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 4);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testShuntCompensatorCreation() {
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

        var createShuntCompensatorEntity1 = ModificationEntity.fromDTO(shunt1);
        var createShuntCompensatorEntity2 = ModificationEntity.fromDTO(shunt2);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createShuntCompensatorEntity1, createShuntCompensatorEntity2));
        assertRequestsCount(2, 3, 0, 0);

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
        assertRequestsCount(5, 0, 0, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 3);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testLineCreation() {
        var createLineEntity1 = ModificationEntity.fromDTO(
                LineCreationInfos.builder().equipmentId("idLine1").equipmentName("nameLine1").r(1.0).x(1.1).g1(10.0).b1(11.0).g2(100.0).b2(100.1).voltageLevelId1("vlId11").busOrBusbarSectionId1(
                        "busId11").voltageLevelId2("vlId12").busOrBusbarSectionId2("busId12").connectionName1("cn11").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2(
                                "cn22").connectionDirection2(ConnectablePosition.Direction.TOP).build());
        var createLineEntity2 = ModificationEntity.fromDTO(
                LineCreationInfos.builder().equipmentId("idLine2").equipmentName("nameLine2").r(2.0).x(2.2).g1(20.0).b1(22.0).g2(200.0).b2(200.2).voltageLevelId1("vlId21").busOrBusbarSectionId1(
                        "busId21").voltageLevelId2("vlId22").busOrBusbarSectionId2("busId22").connectionName1("cn33").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2(
                                "cn44").connectionDirection2(ConnectablePosition.Direction.BOTTOM)
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build())
                            .applicability(SIDE2)
                            .build()
                    )
                )
                .build());
        var createLineEntity3 = ModificationEntity.fromDTO(
                LineCreationInfos.builder().equipmentId("idLine3").equipmentName("nameLine3").r(3.0).x(3.3).g1(30.0).b1(33.0).g2(300.0).b2(300.3).voltageLevelId1("vlId31").busOrBusbarSectionId1(
                        "busId31").voltageLevelId2("vlId32").busOrBusbarSectionId2("busId32").connectionName1("cn55").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2(
                                "cn66").connectionDirection2(ConnectablePosition.Direction.TOP)
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build())
                            .applicability(SIDE1)
                        .build()
                    )
                )
                .build());
        var createLineEntity4 = ModificationEntity.fromDTO(
                LineCreationInfos.builder().equipmentId("idLine4").equipmentName("nameLine4").r(3.0).x(3.3).g1(null).b1(null).g2(null).b2(null).voltageLevelId1("vlId41").busOrBusbarSectionId1(
                        "busId41").voltageLevelId2("vlId42").busOrBusbarSectionId2("busId42").connectionName1("cn77").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2(
                                "cn88").connectionDirection2(ConnectablePosition.Direction.BOTTOM)
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                            CurrentLimitsInfos.builder().permanentLimit(5.0).temporaryLimits(Collections.emptyList()).build())
                            .applicability(SIDE1)
                        .build()
                    )
                )
                .operationalLimitsGroups(
                    List.of(
                        OperationalLimitsGroupInfos.builder().currentLimits(
                                        CurrentLimitsInfos.builder().permanentLimit(4.0).temporaryLimits(Collections.emptyList()).build())
                            .applicability(SIDE2)
                        .build()
                    )
                )
                .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLineEntity1, createLineEntity2, createLineEntity3, createLineEntity4));
        assertRequestsCount(2, 6, 0, 0);

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
        // TODO : Due to an issue the deletion counter is not deterministic
        // https://github.com/jdbc-observations/datasource-proxy/issues/123
        assertRequestsCount(14, 0, 0);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(11, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        // TODO : Due to an issue the deletion counter is not deterministic
        // https://github.com/jdbc-observations/datasource-proxy/issues/123
        assertRequestsCount(12, 0, 0);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testMoveModificationInSameGroup() {
        // use a group Tabular modification
        List<ModificationInfos> groupModifications = List.of(
                GeneratorModificationInfos.builder().equipmentId("idGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v5generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("v6generator").maxP(new AttributeModification<>(500., OperationType.SET)).build(),
                GeneratorModificationInfos.builder().equipmentId("unknownGenerator").maxP(new AttributeModification<>(500., OperationType.SET)).build()
        );
        TabularModificationInfos tabularModificationEntity = TabularModificationInfos.builder()
                .modificationType(ModificationType.GENERATOR_MODIFICATION)
                .modifications(groupModifications)
                .stashed(false)
                .build();
        // and 5 script modifications
        var groovyScriptEntity1 = GroovyScriptInfos.builder().script("script1").build();
        var groovyScriptEntity2 = GroovyScriptInfos.builder().script("script2").build();
        var groovyScriptEntity3 = GroovyScriptInfos.builder().script("script3").build();
        var groovyScriptEntity4 = GroovyScriptInfos.builder().script("script4").build();
        var groovyScriptEntity5 = GroovyScriptInfos.builder().script("script5").build();

        List<ModificationInfos> modifications = List.of(groovyScriptEntity1, groovyScriptEntity2, groovyScriptEntity3, groovyScriptEntity4, groovyScriptEntity5, tabularModificationEntity);
        networkModificationRepository.saveModificationInfos(TEST_GROUP_ID, modifications);
        assertRequestsCount(2, 8, 0, 0);

        var modificationOriginal = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                List.of(modificationOriginal.get(5).getUuid()), modificationOriginal.get(1).getUuid());
        assertRequestsCount(6, 0, 2, 0);

        var modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        // [0:1, 1:6, 2:2, 3:3, 4:4 ,5:5 ]
        var expected = List.of(modificationOriginal.get(0), modificationOriginal.get(5),
            modificationOriginal.get(1), modificationOriginal.get(2), modificationOriginal.get(3), modificationOriginal.get(4));

        assertEquals(getIds(expected), getIds(modification));

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                List.of(modificationOriginal.get(2).getUuid(), modificationOriginal.get(5).getUuid()), null);
        assertRequestsCount(6, 0, 2, 0);

        // [0:1, 1:2, 2:4, 3:5, 4:6, 5:3 ]
        modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        expected = List.of(modificationOriginal.get(0), modificationOriginal.get(1), modificationOriginal.get(3),
            modificationOriginal.get(4), modificationOriginal.get(2), modificationOriginal.get(5));
        assertEquals(getIds(expected), getIds(modification));

    }

    @Test
    void testMoveModificationsBetweenTwoGroups() {
        var groovyScriptEntity1 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script1").build());
        var groovyScriptEntity2 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script2").build());
        var groovyScriptEntity3 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script3").build());
        var groovyScriptEntity4 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script4").build());
        var groovyScriptEntity5 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script5").build());
        var groovyScriptEntity6 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script6").build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2,
                groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(2, 3, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(2, 3, 0, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        List<UUID> uuidsToMove = List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId());

        List<ModificationInfos> movedModifications = networkModificationRepository.moveModifications(
            new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
            new ModificationContainerInfos(TEST_GROUP_ID_2, ModificationContainerType.GROUP),
            uuidsToMove, null);
        assertEquals(uuidsToMove.size(), movedModifications.size());
        assertRequestsCount(5, 0, 1, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        var expected1 = List.of(modificationOriginal1.get(0), modificationOriginal1.get(3));
        var expected2 = List.of(modificationOriginal2.get(0), modificationOriginal2.get(1), modificationOriginal1.get(1), modificationOriginal1.get(2));

        assertEquals(getIds(expected1), getIds(modification1));
        assertEquals(getIds(expected2), getIds(modification2));

        // cutting and pasting to non existing group should work (the destination group is implicitly created)
        SQLStatementCountValidator.reset();
        uuidsToMove = List.of(expected2.get(0).getUuid(), expected2.get(1).getUuid());
        movedModifications = networkModificationRepository.moveModifications(
            new ModificationContainerInfos(TEST_GROUP_ID_2, ModificationContainerType.GROUP),
            new ModificationContainerInfos(TEST_GROUP_ID_3, ModificationContainerType.GROUP),
            uuidsToMove, null);
        assertEquals(uuidsToMove.size(), movedModifications.size());
        assertRequestsCount(4, 1, 1, 0);

        modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modification3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        expected2 = List.of(modificationOriginal1.get(1), modificationOriginal1.get(2));
        var expected3 = List.of(modificationOriginal2.get(0), modificationOriginal2.get(1));

        assertEquals(getIds(modification2), getIds(expected2));
        assertEquals(getIds(expected3), getIds(modification3));
    }

    @Test
    void testMoveModificationsBetweenTwoGroupsWithReferenceNode() {
        var groovyScriptEntity1 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script1").build());
        var groovyScriptEntity2 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script2").build());
        var groovyScriptEntity3 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script3").build());
        var groovyScriptEntity4 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script4").build());
        var groovyScriptEntity5 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script5").build());
        var groovyScriptEntity6 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script6").build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2,
                groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(2, 3, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(2, 3, 0, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        List<UUID> uuidsToMove = List.of(groovyScriptEntity2.getId(), groovyScriptEntity3.getId());
        List<ModificationInfos> movedModifications = networkModificationRepository.moveModifications(
            new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
            new ModificationContainerInfos(TEST_GROUP_ID_2, ModificationContainerType.GROUP),
            uuidsToMove, groovyScriptEntity6.getId());
        assertEquals(uuidsToMove.size(), movedModifications.size());
        assertRequestsCount(5, 0, 1, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        var expected1 = List.of(modificationOriginal1.get(0), modificationOriginal1.get(3));
        var expected2 = List.of(modificationOriginal2.get(0), modificationOriginal1.get(1), modificationOriginal1.get(2), modificationOriginal2.get(1));

        assertEquals(getIds(expected1), getIds(modification1));
        assertEquals(getIds(expected2), getIds(modification2));
    }

    @Test
    void testMoveModificationsBetweenMoreThanTwoGroups() {
        var groovyScriptEntity1 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script1").build());
        var groovyScriptEntity2 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script2").build());
        var groovyScriptEntity3 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script3").build());
        var groovyScriptEntity4 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script4").build());
        var groovyScriptEntity5 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script5").build());
        var groovyScriptEntity6 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script6").build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2));
        assertRequestsCount(2, 3, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptEntity3, groovyScriptEntity4));
        assertRequestsCount(2, 3, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(groovyScriptEntity5, groovyScriptEntity6));
        assertRequestsCount(2, 3, 0, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modificationOriginal3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        // moving modifications with a good and a bad modification should work (the bad one will be ignored)
        SQLStatementCountValidator.reset();
        List<UUID> modificationsToMoveUuid = List.of(groovyScriptEntity1.getId(), UUID.randomUUID());
        List<ModificationInfos> movedModifications = networkModificationRepository.moveModifications(
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                new ModificationContainerInfos(TEST_GROUP_ID_3, ModificationContainerType.GROUP),
                modificationsToMoveUuid, null);
        assertRequestsCount(5, 0, 1, 0);
        // only the valid modification is moved
        assertEquals(1, movedModifications.size());
        assertEquals(groovyScriptEntity1.getId(), movedModifications.get(0).getUuid());

        // try to move again: empty result cause groovyScriptEntity1 has been moved
        SQLStatementCountValidator.reset();
        List<ModificationInfos> movedModifications2 = networkModificationRepository.moveModifications(
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                new ModificationContainerInfos(TEST_GROUP_ID_3, ModificationContainerType.GROUP),
                modificationsToMoveUuid, null);
        assertRequestsCount(3, 0, 0, 0);
        assertEquals(0, movedModifications2.size());

        // moving modification with reference node not in destination: exception expected
        SQLStatementCountValidator.reset();
        ModificationContainerInfos source = new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP);
        ModificationContainerInfos target = new ModificationContainerInfos(TEST_GROUP_ID_2, ModificationContainerType.GROUP);
        List<UUID> modificationsToMoveUuid2 = List.of(groovyScriptEntity2.getId());
        UUID referenceNodeUuid = groovyScriptEntity2.getId();
        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.moveModifications(
                source, target,
                modificationsToMoveUuid2, referenceNodeUuid),
                new NetworkModificationServerException(MOVE_COMPOSITE_MODIFICATION_CYCLE_ERROR).getMessage());
        assertRequestsCount(5, 0, 0, 0);

        var modification1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modification2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modification3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        var expected1 = List.of(modificationOriginal1.get(1));
        var expected3 = List.of(modificationOriginal3.get(0), modificationOriginal3.get(1), modificationOriginal1.get(0));

        assertEquals(getIds(modification1), getIds(expected1));
        assertEquals(getIds(modification2), getIds(modificationOriginal2));
        assertEquals(getIds(modification3), getIds(expected3));
    }

    private static List<UUID> getIds(List<ModificationInfos> expected) {
        return expected.stream().map(ModificationInfos::getUuid).collect(Collectors.toList());
    }

    @Test
    void testGroovyScript() {
        var groovyScriptEntity1 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script1").build());
        var groovyScriptEntity2 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script2").build());
        var groovyScriptEntity3 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("script3").build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptEntity1, groovyScriptEntity2, groovyScriptEntity3));
        assertRequestsCount(2, 3, 0, 0);

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
        assertRequestsCount(4, 0, 0, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(4, 0, 0, 3);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testSubstationCreation() {
        var createSubstationEntity1 = ModificationEntity.fromDTO(SubstationCreationInfos.builder()
                .equipmentId("idSubstation1")
                .equipmentName("nameSubstation1")
                .country(Country.FR)
                .properties(List.of(FreePropertyInfos.builder().name("DEMO").value("DemoU").build()))
                .build());
        var createSubstationEntity2 = ModificationEntity.fromDTO(SubstationCreationInfos.builder()
                .equipmentId("idSubstation2")
                .equipmentName("nameSubstation2")
                .country(Country.TD)
                .properties(null)
                .build());
        var createSubstationEntity3 = ModificationEntity.fromDTO(SubstationCreationInfos.builder()
                .equipmentId("idSubstation3")
                .equipmentName("nameSubstation3")
                .country(Country.KG)
                .properties(null)
                .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createSubstationEntity1, createSubstationEntity2, createSubstationEntity3));
        assertRequestsCount(2, 4, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getSubstationCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createSubstationEntity1.toModificationInfos());
        assertThat(getSubstationCreationModification(modificationInfos.get(1).getUuid()))
            .recursivelyEquals(createSubstationEntity2.toModificationInfos());
        assertThat(getSubstationCreationModification(modificationInfos.get(2).getUuid()))
            .recursivelyEquals(createSubstationEntity3.toModificationInfos());

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createSubstationEntity2.getId(), createSubstationEntity3.getId()));
        assertRequestsCount(6, 0, 0, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 4);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testVoltageLevelCreation() {
        var createVoltLvlEntity1 = ModificationEntity.fromDTO(VoltageLevelCreationInfos.builder()
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
                .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1));
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, modificationInfos.size());

        assertThat(getVoltageLevelCreationModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(createVoltLvlEntity1.toModificationInfos());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1.getId()));
        assertRequestsCount(6, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    private static VoltageLevelCreationInfos makeAVoltageLevelInfos() {
        return VoltageLevelCreationInfos.builder()
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
    }

    @Test
    void testStatusLineModification() {
        var entities = List.of(
            ModificationEntity.fromDTO(OperatingStatusModificationInfos.builder().equipmentId("idLine1").action(OperatingStatusModificationInfos.ActionType.LOCKOUT).build()),
            ModificationEntity.fromDTO(OperatingStatusModificationInfos.builder().equipmentId("idLine2").action(OperatingStatusModificationInfos.ActionType.TRIP).build()),
            ModificationEntity.fromDTO(OperatingStatusModificationInfos.builder().equipmentId("idLine3").action(OperatingStatusModificationInfos.ActionType.SWITCH_ON).build()),
            ModificationEntity.fromDTO(OperatingStatusModificationInfos.builder().equipmentId("idLine4").action(OperatingStatusModificationInfos.ActionType.ENERGISE_END_ONE).build()),
            ModificationEntity.fromDTO(OperatingStatusModificationInfos.builder().equipmentId("idLine5").action(OperatingStatusModificationInfos.ActionType.ENERGISE_END_TWO).build())
        );

        networkModificationRepository.saveModifications(TEST_GROUP_ID, entities);
        assertRequestsCount(2, 3, 0, 0);

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
        // n+1 query because we are deleting modifications 1 by 1, it's for now accepted according to a comment in "deleteModificationGroup"
        assertRequestsCount(9, 0, 0, 3);
    }

    @Test
    void testLineSplitWithVoltageLevel() {
        var lineSplitEntity1 = ModificationEntity.fromDTO(LineSplitWithVoltageLevelInfos.builder()
            .lineToSplitId("lineId0")
            .percent(30.0)
            .mayNewVoltageLevelInfos(null)
            .existingVoltageLevelId("vl1")
            .bbsOrBusId("bbsId")
            .newLine1Id("line1id")
            .newLine1Name("line1Name")
            .newLine2Id("line2Id")
            .newLine2Name("line2Name")
            .build());
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos();
        var lineSplitEntity2 = ModificationEntity.fromDTO(LineSplitWithVoltageLevelInfos.builder()
            .lineToSplitId("lineId1")
            .percent(30.0)
            .mayNewVoltageLevelInfos(voltageLevelCreationInfos)
            .existingVoltageLevelId(null)
            .bbsOrBusId("bbsId")
            .newLine1Id("line1id")
            .newLine1Name("line1Name")
            .newLine2Id("line2Id")
            .newLine2Name("line2Name")
            .build());
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
        assertRequestsCount(8, 0, 0);

        modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(0, modificationInfos.size());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testLineAttachToVoltageLevel() {
        LineCreationInfos attachmentLine = LineCreationInfos.builder()
                .equipmentId("attachmentLineId")
                .r(50.6)
                .x(25.3)
                .build();
        var lineAttachToEntity1 = ModificationEntity.fromDTO(LineAttachToVoltageLevelInfos.builder()
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
                .build());
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos();
        var lineAttachToEntity2 = ModificationEntity.fromDTO(LineAttachToVoltageLevelInfos.builder()
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
                .build());
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
        assertRequestsCount(12, 0, 0, 12);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testLinesAttachToSplitLines() {
        var linesAttachToEntity1 = ModificationEntity.fromDTO(LinesAttachToSplitLinesInfos.builder()
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .attachedLineId("lineId3")
                .voltageLevelId("vl1")
                .bbsBusId("bbsId")
                .replacingLine1Id("line1Id")
                .replacingLine1Name("line1Name")
                .replacingLine2Id("line2Id")
                .replacingLine2Name("line2Name")
                .build());
        var linesAttachToEntity2 = ModificationEntity.fromDTO(LinesAttachToSplitLinesInfos.builder()
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .attachedLineId("lineId6")
                .voltageLevelId("vl2")
                .bbsBusId("bbsId2")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .replacingLine2Id("line4Id")
                .replacingLine2Name("line4Name")
                .build());
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
        assertRequestsCount(4, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testDeleteStashedModificationList() {
        //create a modification and add it to the repository
        var groovyScriptEntity1 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().stashed(true).script("script1").build());
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
    void testDeleteNonStashedModificationList() {
        //create a modification and add it to the repository
        var groovyScriptEntity1 = ModificationEntity.fromDTO(GroovyScriptInfos.builder().stashed(false).script("script1").build());
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
    void testDeleteAttachingLine() {
        var deleteAttachingLineEntity = ModificationEntity.fromDTO(DeleteAttachingLineInfos.builder()
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .attachedLineId("lineId3")
                .replacingLine1Id("vl1")
                .replacingLine1Name("line1Name")
                .build());

        var deleteAttachingLineEntity2 = ModificationEntity.fromDTO(DeleteAttachingLineInfos.builder()
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .attachedLineId("lineId6")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(deleteAttachingLineEntity, deleteAttachingLineEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(deleteAttachingLineEntity.getId(),
                deleteAttachingLineEntity2.getId()));
        assertRequestsCount(4, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testDeleteVoltageLevelOnLine() {
        var deleteVoltageLevelOnLineToEntity1 = ModificationEntity.fromDTO(DeleteVoltageLevelOnLineInfos.builder()
                .lineToAttachTo1Id("lineId0")
                .lineToAttachTo2Id("lineId1")
                .replacingLine1Id("line1Id")
                .replacingLine1Name("line1Name")
                .build());

        var deleteVoltageLevelOnLineToEntity2 = ModificationEntity.fromDTO(DeleteVoltageLevelOnLineInfos.builder()
                .lineToAttachTo1Id("lineId4")
                .lineToAttachTo2Id("lineId5")
                .replacingLine1Id("line3Id")
                .replacingLine1Name("line3Name")
                .build());

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
        assertRequestsCount(4, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    private static <T> void testModificationEmbedded(IAttributeModificationEmbeddable<T> modification, T val) {
        assertEquals(val, modification.getValue());
        assertEquals(OperationType.SET, modification.getOpType());
    }

    @Test
    void testEmbeddedModificationTypes() {
        testModificationEmbedded(new DoubleModificationEmbedded(new AttributeModification<>(10., OperationType.SET)), 10.);
        testModificationEmbedded(new EnumModificationEmbedded<>(new AttributeModification<>(OperationType.SET, OperationType.SET)), OperationType.SET);
        testModificationEmbedded(new BooleanModificationEmbedded(new AttributeModification<>(true, OperationType.SET)), true);
    }

    @Test
    void testVoltageInitModification() {
        var voltageInitModificationEntity = ModificationEntity.fromDTO(VoltageInitModificationInfos.builder()
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
                    .ratioTapChangerTargetV(225.)
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
                    .targetV(225.)
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
                    .targetV(380.)
                    .build()))
            .buses(List.of(
                VoltageInitBusModificationInfos.builder()
                    .voltageLevelId("1")
                    .busId("B1")
                    .v(225.)
                    .angle(0.)
                    .build(),
                VoltageInitBusModificationInfos.builder()
                    .voltageLevelId("2")
                    .busId("B2")
                    .v(380.)
                    .angle(0.3)
                    .build()))
            .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(voltageInitModificationEntity));
        assertRequestsCount(2, 9, 0, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());

        assertThat(getVoltageInitModification(modificationInfos.get(0).getUuid()))
            .recursivelyEquals(voltageInitModificationEntity.toModificationInfos());

        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(voltageInitModificationEntity.getId()));
        assertRequestsCount(4, 0, 0, 8);

        SQLStatementCountValidator.reset();
        assertEquals(0, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);
    }

    @Test
    void testVscModification() {
        var vscModificationEntity = ModificationEntity.fromDTO(VscModificationInfos.builder()
            .equipmentId("VSC1")
                .converterStation1(ConverterStationModificationInfos.builder().equipmentId("C1").build())
                .converterStation2(ConverterStationModificationInfos.builder().equipmentId("C2").build())
            .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(vscModificationEntity));
        assertRequestsCount(2, 5, 0, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());
    }

    @Test
    void testLccModification() {
        var lccModificationEntity = ModificationEntity.fromDTO(LccModificationInfos.builder()
            .equipmentId("LCC1")
            .converterStation1(LccConverterStationModificationInfos.builder().equipmentId("C1").build())
            .converterStation2(LccConverterStationModificationInfos.builder().equipmentId("C2").build())
            .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lccModificationEntity));
        assertRequestsCount(2, 5, 0, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());
    }

    @Test
    void testGetModificationCount() {
        var modifEntity1 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        var modifEntity2 = ModificationEntity.fromDTO(
                EquipmentAttributeModificationInfos.builder().equipmentId("id2").equipmentAttributeName("attribute").equipmentAttributeValue("foo").equipmentType(IdentifiableType.VOLTAGE_LEVEL).build(
                        ));
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));
        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModificationsCount(TEST_GROUP_ID, false).intValue());
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        assertEquals(0, networkModificationRepository.getModificationsCount(TEST_GROUP_ID, true).intValue());
        assertRequestsCount(1, 0, 0, 0);

        assertThrows(NullPointerException.class, () -> networkModificationRepository.getModificationsCount(null, true));
    }

    @Test
    void testModificationOrder() {
        // add 1 modification in a group
        var modifEntity1 = ModificationEntity.fromDTO(
            EquipmentAttributeModificationInfos.builder()
                .equipmentId("id1")
                .equipmentAttributeName("attribute")
                .equipmentAttributeValue("foo")
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
                .build());
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1));
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of());
        // move it in another group
        List<ModificationInfos> movedEntities = networkModificationRepository.moveModifications(
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                new ModificationContainerInfos(TEST_GROUP_ID_2, ModificationContainerType.GROUP),
                List.of(modifEntity1.getId()),
                null);
        assertEquals(1, movedEntities.size());
        ModificationEntity entity1 = modificationRepository.findById(movedEntities.get(0).getUuid()).orElseThrow();
        assertEquals(0, entity1.getModificationsOrder());

        // put another modification in empty origin group: its order must restart to 0 as well
        var modifEntity2 = ModificationEntity.fromDTO(
            EquipmentAttributeModificationInfos.builder()
                .equipmentId("id2")
                .equipmentAttributeName("attribute")
                .equipmentAttributeValue("foo")
                .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
                .build());
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity2));
        // trick: move it too, to see the order in the entity
        movedEntities = networkModificationRepository.moveModifications(
                new ModificationContainerInfos(TEST_GROUP_ID, ModificationContainerType.GROUP),
                new ModificationContainerInfos(TEST_GROUP_ID_2, ModificationContainerType.GROUP),
                List.of(modifEntity2.getId()),
                null);
        assertEquals(1, movedEntities.size());
        ModificationEntity entity2 = modificationRepository.findById(movedEntities.get(0).getUuid()).orElseThrow();
        assertEquals(1, entity2.getModificationsOrder());
    }

    @Test
    void testStaticVarCompensatorCreation() {
        var createStaticVarCompensator1 = ModificationEntity.fromDTO(StaticVarCompensatorCreationInfos.builder()
                .equipmentId("idStaticVarCompensator1").equipmentName("nameStaticVarCompensator1")
                .voltageLevelId("vlId1")
                .busOrBusbarSectionId("busId1")
                .minSusceptance(200.0)
                .maxSusceptance(224.0)
                .regulationMode(VOLTAGE)
                .voltageSetpoint(200.0)
                .voltageRegulationType(DISTANT)
                .regulatingTerminalId("testTerminalId1")
                .regulatingTerminalType("STATIC_VAR_COMPENSATOR").regulatingTerminalVlId("idVlTest1")
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(1).build());
        var createStaticVarCompensator2 = ModificationEntity.fromDTO(StaticVarCompensatorCreationInfos.builder()
                .equipmentId("idStaticVarCompensator2").equipmentName("nameStaticVarCompensator2")
                .voltageLevelId("vlId2")
                .busOrBusbarSectionId("busId2")
                .regulatingTerminalId(null)
                .regulatingTerminalType(null).regulatingTerminalVlId("idVlTest2")
                .connectionName("Bot").connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition(2).build());
        var createStaticVarCompensator3 = ModificationEntity.fromDTO(StaticVarCompensatorCreationInfos.builder()
                .equipmentId("idStaticVarCompensator3").equipmentName("nameStaticVarCompensator3")
                .voltageLevelId("vlId2")
                .busOrBusbarSectionId("busId2")
                .regulatingTerminalId(null)
                .regulatingTerminalType(null).regulatingTerminalVlId("idVlTest2")
                .connectionName("Bot").connectionDirection(ConnectablePosition.Direction.BOTTOM)
                .connectionPosition(2)
                .regulationMode(VOLTAGE)
                .standbyAutomatonOn(true)
                .standby(true)
                .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createStaticVarCompensator1, createStaticVarCompensator2, createStaticVarCompensator3));
        assertRequestsCount(2, 3, 0, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getStaticVarCompensatorCreationModification(modificationInfos.get(0).getUuid()))
                .recursivelyEquals(createStaticVarCompensator1.toModificationInfos());
        assertThat(getStaticVarCompensatorCreationModification(modificationInfos.get(1).getUuid()))
                .recursivelyEquals(createStaticVarCompensator2.toModificationInfos());

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createStaticVarCompensator2.getId(), createStaticVarCompensator3.getId()));
        assertRequestsCount(6, 0, 0, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(3, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 3);

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true),
            new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    void testVoltageLevelTopologyModification() {
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfos = new ArrayList<>(
                Arrays.asList(
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("sw1")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build()
                )
        );
        var voltageLevelTopologyModificationEntity = ModificationEntity.fromDTO(VoltageLevelTopologyModificationInfos.builder()
                .equipmentId("VL1")
                .equipmentAttributeModificationList(equipmentAttributeModificationInfos)
                .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(voltageLevelTopologyModificationEntity));
        assertRequestsCount(2, 5, 1, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());
    }

    @Test
    void testCreateCouplingDevice() {
        ModificationEntity modification = ModificationEntity.fromDTO(CreateCouplingDeviceInfos.builder()
            .couplingDeviceInfos(CouplingDeviceInfos.builder()
                .busbarSectionId1("bbs1")
                .busbarSectionId2("bbs2")
                .build())
            .build());

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modification));
        assertRequestsCount(2, 3, 0, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, modificationInfos.size());
    }

    private static ModificationEntity switchModification(String equipmentId) {
        return ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder()
                .equipmentId(equipmentId).equipmentAttributeName("open").equipmentAttributeValue(true)
                .equipmentType(IdentifiableType.SWITCH).build());
    }

    private UUID insertComposite(UUID targetGroupUuid, boolean shared, String... equipmentIds) {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID,
                Arrays.stream(equipmentIds).map(ModificationRepositoryTest::switchModification).toList());
        UUID compositeUuid = networkModificationRepository.createNetworkCompositeModification(
                modifications.stream().map(ModificationInfos::getUuid).toList(), "composite");
        return networkModificationRepository.insertCompositeModifications(targetGroupUuid,
                List.of(new CompositeInfos(compositeUuid, "composite", shared, "description"))).getFirst().getUuid();
    }

    /**
     * Reads the applicability of the modifications of a group the way the front end does: through the metadata,
     * where every modification carries its own.
     */
    private Map<UUID, Map<String, Boolean>> getApplicabilities(UUID groupUuid) {
        return networkModificationRepository.getModifications(groupUuid, true, true).stream()
                .collect(Collectors.toMap(ModificationInfos::getUuid, ModificationInfos::getApplicabilityByRootNetworkTag));
    }

    private Map<UUID, Map<String, Boolean>> getApplicabilitiesByModificationsInside(UUID containerUuid) {
        return networkModificationRepository.getBasicNetworkModificationsFromComposite(List.of(containerUuid)).stream()
                .collect(Collectors.toMap(ModificationInfos::getUuid, ModificationInfos::getApplicabilityByRootNetworkTag));
    }

    @Test
    void testUpdateRootNetworkApplicability() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID,
                List.of(switchModification("v1d1"), switchModification("v1d2")));
        UUID deactivatedUuid = modifications.get(0).getUuid();
        UUID untouchedUuid = modifications.get(1).getUuid();

        networkModificationRepository.updateRootNetworkApplicability(List.of(deactivatedUuid), ROOT_NETWORK_TAG, false);

        Map<UUID, Map<String, Boolean>> applicabilities = getApplicabilities(TEST_GROUP_ID);
        assertEquals(Map.of(ROOT_NETWORK_TAG, false), applicabilities.get(deactivatedUuid));
        assertEquals(Map.of(), applicabilities.get(untouchedUuid),
                "A modification never updated holds no applicability, which means applicable everywhere");

        // reactivating writes an explicit entry rather than removing it
        networkModificationRepository.updateRootNetworkApplicability(List.of(deactivatedUuid), ROOT_NETWORK_TAG, true);
        assertEquals(Map.of(ROOT_NETWORK_TAG, true), getApplicabilities(TEST_GROUP_ID).get(deactivatedUuid));
    }

    @Test
    void testCopyingAModificationKeepsItsApplicabilities() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(switchModification("v1d1")));
        UUID modificationUuid = modifications.getFirst().getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(modificationUuid), ROOT_NETWORK_TAG, false);

        // copying a modification goes through its infos, which have to carry the applicabilities
        SQLStatementCountValidator.reset();
        UUID compositeUuid = networkModificationRepository.createNetworkCompositeModification(List.of(modificationUuid), "composite");
        SQLStatementCountValidator.assertSelectCount(2);

        CompositeModificationInfos composite = (CompositeModificationInfos) networkModificationRepository.getModificationInfo(compositeUuid);
        UUID copyUuid = composite.getModificationsInfos().getFirst().getUuid();
        assertEquals(List.of(new ModificationApplicability(copyUuid, ROOT_NETWORK_TAG, false)),
                modificationRepository.findApplicabilitiesByIdIn(List.of(copyUuid)),
                "The copy carries the applicability of the modification it was made from");
    }

    @Test
    void testDeletingAModificationDeletesItsApplicabilities() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(switchModification("v1d1")));
        UUID modificationUuid = modifications.getFirst().getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(modificationUuid), ROOT_NETWORK_TAG, false);

        // the applicabilities are deleted by the database, so a modification carrying one is deleted like any other
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(modificationUuid));

        assertEquals(List.of(), modificationRepository.findApplicabilitiesByIdIn(List.of(modificationUuid)),
                "The applicabilities of a deleted modification must not outlive it");
    }

    @Test
    void testUpdateRootNetworkApplicabilityKeepsTagsIndependent() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(switchModification("v1d1")));
        UUID modificationUuid = modifications.getFirst().getUuid();

        networkModificationRepository.updateRootNetworkApplicability(List.of(modificationUuid), ROOT_NETWORK_TAG, false);
        networkModificationRepository.updateRootNetworkApplicability(List.of(modificationUuid), OTHER_ROOT_NETWORK_TAG, true);

        assertEquals(Map.of(ROOT_NETWORK_TAG, false, OTHER_ROOT_NETWORK_TAG, true),
                getApplicabilities(TEST_GROUP_ID).get(modificationUuid));
    }

    @Test
    void testUpdateRootNetworkApplicabilityPropagatesToCompositeSubModifications() {
        UUID compositeUuid = insertComposite(TEST_GROUP_ID_2, false, "v1d1", "v1d2");

        networkModificationRepository.updateRootNetworkApplicability(List.of(compositeUuid), ROOT_NETWORK_TAG, false);

        assertEquals(Map.of(ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_2).get(compositeUuid));
        Map<UUID, Map<String, Boolean>> applicabilitiesByModifications = getApplicabilitiesByModificationsInside(compositeUuid);
        assertEquals(2, applicabilitiesByModifications.size());
        applicabilitiesByModifications.values().forEach(applicability ->
                assertEquals(Map.of(ROOT_NETWORK_TAG, false), applicability,
                        "Updating a composite must reach its sub modifications"));
    }

    @Test
    void testUpdateRootNetworkApplicabilityOnSharedModification() {
        UUID referenceUuid = insertComposite(TEST_GROUP_ID_2, true, "v1d1");
        UUID sharedUuid = ((ModificationReferenceInfos) networkModificationRepository.getModificationInfo(referenceUuid)).getReferenceId();

        networkModificationRepository.updateRootNetworkApplicability(List.of(referenceUuid), ROOT_NETWORK_TAG, false);

        assertEquals(List.of(Map.of(ROOT_NETWORK_TAG, false)), List.copyOf(getApplicabilitiesByModificationsInside(sharedUuid).values()),
                "A reference has no applicability of its own: the update went to the shared modification it points to");
        assertEquals(Map.of(ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_2).get(referenceUuid),
                "The reference is reported with the applicability of the shared modification");
    }

    @Test
    void testGetActiveModificationsLeavesOutWhatTheTagDeactivates() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID,
                List.of(switchModification("v1d1"), switchModification("v1d2")));
        UUID deactivatedUuid = modifications.get(0).getUuid();
        UUID keptUuid = modifications.get(1).getUuid();

        networkModificationRepository.updateRootNetworkApplicability(List.of(deactivatedUuid), ROOT_NETWORK_TAG, false);

        assertEquals(List.of(keptUuid), activeModificationUuids(TEST_GROUP_ID, ROOT_NETWORK_TAG),
                "A modification the tag deactivates is excluded from active modifications");
        assertEquals(List.of(deactivatedUuid, keptUuid), activeModificationUuids(TEST_GROUP_ID, OTHER_ROOT_NETWORK_TAG),
                "Only an explicit false entry for the tag leaves a modification out, and this tag has none");
        assertEquals(List.of(deactivatedUuid, keptUuid), activeModificationUuids(TEST_GROUP_ID, null),
                "Without a root network context the applicabilities are ignored");
    }

    @Test
    void testGetActiveModificationsFiltersAReferenceOnItsSharedModification() {
        // inserting a composite as shared puts a reference to it in the group, not a copy of it
        UUID referenceUuid = insertComposite(TEST_GROUP_ID_2, true, "v1d1");
        UUID sharedUuid = ((ModificationReferenceInfos) networkModificationRepository.getModificationInfo(referenceUuid)).getReferenceId();

        networkModificationRepository.updateRootNetworkApplicability(List.of(sharedUuid), ROOT_NETWORK_TAG, false);

        assertEquals(List.of(), activeModificationUuids(TEST_GROUP_ID_2, ROOT_NETWORK_TAG),
                "Deactivating the shared modification also deactivate the reference, which holds no applicability of its own");
        assertEquals(List.of(referenceUuid), activeModificationUuids(TEST_GROUP_ID_2, OTHER_ROOT_NETWORK_TAG),
                "Only an explicit false entry on the shared modification leaves the reference out, and this tag has none");
    }

    @Test
    void testGetActiveModificationsLeavesOutTheChildrenOfADeactivatedComposite() {
        UUID compositeUuid = insertComposite(TEST_GROUP_ID_2, false, "v1d1", "v1d2");
        networkModificationRepository.updateRootNetworkApplicability(List.of(compositeUuid), ROOT_NETWORK_TAG, false);
        UUID childUuid = networkModificationRepository.getBasicNetworkModificationsFromComposite(List.of(compositeUuid)).getFirst().getUuid();

        // we activate a child while the composite remains deactivated
        networkModificationRepository.updateRootNetworkApplicability(List.of(childUuid), ROOT_NETWORK_TAG, true);

        assertEquals(Map.of(ROOT_NETWORK_TAG, true), getApplicabilitiesByModificationsInside(compositeUuid).get(childUuid),
                "Taken on its own the child is applicable on the tag");
        assertEquals(List.of(), activeModificationUuids(TEST_GROUP_ID_2, ROOT_NETWORK_TAG),
                "A deactivated composite automatically deactivate its children whatever their own applicability is");
    }

    @Test
    void testGetActiveModificationsLeavesOutWhatTheTagDeactivatesInsideAComposite() {
        insertComposite(TEST_GROUP_ID_2, false, "v1d1", "v1d2");
        List<UUID> contentUuids = activeCompositeContentUuids(TEST_GROUP_ID_2, null);
        UUID deactivatedUuid = contentUuids.get(0);
        UUID keptUuid = contentUuids.get(1);

        networkModificationRepository.updateRootNetworkApplicability(List.of(deactivatedUuid), ROOT_NETWORK_TAG, false);

        assertEquals(List.of(keptUuid), activeCompositeContentUuids(TEST_GROUP_ID_2, ROOT_NETWORK_TAG),
                "A modification the tag deactivates is dropped from the composite holding it");
        assertEquals(contentUuids, activeCompositeContentUuids(TEST_GROUP_ID_2, OTHER_ROOT_NETWORK_TAG),
                "Only an explicit false entry for the tag drops a modification, and this tag has none");
        assertEquals(contentUuids, activeCompositeContentUuids(TEST_GROUP_ID_2, null),
                "Without a root network context the applicabilities are ignored");
    }

    @Test
    void testUpdatingTheApplicabilityDoesNotLoadTheModificationsOneByOne() {
        List<UUID> oneModification = saveSwitchModifications("single", 1);
        List<UUID> manyModifications = saveSwitchModifications("many", 20);

        SQLStatementCountValidator.reset();
        networkModificationRepository.updateRootNetworkApplicability(oneModification, ROOT_NETWORK_TAG, false);
        assertRequestsCount(1, 1, 0, 0);

        // twenty modifications cost what one costs: they are loaded in one query, not one by one
        SQLStatementCountValidator.reset();
        networkModificationRepository.updateRootNetworkApplicability(manyModifications, ROOT_NETWORK_TAG, false);
        assertRequestsCount(1, 1, 0, 0);
    }

    private List<UUID> saveSwitchModifications(String equipmentIdPrefix, int count) {
        return networkModificationRepository.saveModifications(TEST_GROUP_ID_2,
                        IntStream.range(0, count).mapToObj(i -> switchModification(equipmentIdPrefix + i)).toList())
                .stream().map(ModificationInfos::getUuid).toList();
    }

    @Test
    void testRenameRootNetworkTagMovesTheApplicabilityOfTheGroupModifications() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID_3,
                List.of(switchModification("v1d1"), switchModification("v1d2")));
        UUID deactivatedUuid = modifications.get(0).getUuid();
        UUID untouchedUuid = modifications.get(1).getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(deactivatedUuid), ROOT_NETWORK_TAG, false);

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_3), ROOT_NETWORK_TAG, RENAMED_ROOT_NETWORK_TAG);

        Map<UUID, Map<String, Boolean>> applicabilities = getApplicabilities(TEST_GROUP_ID_3);
        assertEquals(Map.of(RENAMED_ROOT_NETWORK_TAG, false), applicabilities.get(deactivatedUuid),
                "Renaming the tag of a root network must keep its applicability");
        assertEquals(Map.of(), applicabilities.get(untouchedUuid));
        assertEquals(List.of(untouchedUuid), activeModificationUuids(TEST_GROUP_ID_3, RENAMED_ROOT_NETWORK_TAG));
    }

    @Test
    void testRenameRootNetworkTagReachesTheContentOfAComposite() {
        UUID compositeUuid = insertComposite(TEST_GROUP_ID_2, false, "v1d1", "v1d2");
        networkModificationRepository.updateRootNetworkApplicability(List.of(compositeUuid), ROOT_NETWORK_TAG, false);

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_2), ROOT_NETWORK_TAG, RENAMED_ROOT_NETWORK_TAG);

        assertEquals(Map.of(RENAMED_ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_2).get(compositeUuid));
        Map<UUID, Map<String, Boolean>> applicabilitiesByModifications = getApplicabilitiesByModificationsInside(compositeUuid);
        assertEquals(2, applicabilitiesByModifications.size());
        applicabilitiesByModifications.values().forEach(applicability ->
                assertEquals(Map.of(RENAMED_ROOT_NETWORK_TAG, false), applicability,
                        "A composite propagates its applicability to its content, so the rename must reach it too"));
    }

    @Test
    void testRenameRootNetworkTagOnlyAddsToASharedModification() {
        UUID referenceUuid = insertComposite(TEST_GROUP_ID_2, true, "v1d1");
        UUID sharedUuid = ((ModificationReferenceInfos) networkModificationRepository.getModificationInfo(referenceUuid)).getReferenceId();
        networkModificationRepository.updateRootNetworkApplicability(List.of(referenceUuid), ROOT_NETWORK_TAG, false);

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_2), ROOT_NETWORK_TAG, RENAMED_ROOT_NETWORK_TAG);

        Map<String, Boolean> bothTags = Map.of(ROOT_NETWORK_TAG, false, RENAMED_ROOT_NETWORK_TAG, false);
        assertEquals(bothTags, getApplicabilities(TEST_GROUP_ID_2).get(referenceUuid),
                "We don't replace the old tag in shared modifications, the new one is just added");
        assertEquals(List.of(bothTags), List.copyOf(getApplicabilitiesByModificationsInside(sharedUuid).values()),
                "The content of the shared composite carries the applicability too, so the rename must reach it");
        assertEquals(List.of(), activeModificationUuids(TEST_GROUP_ID_2, RENAMED_ROOT_NETWORK_TAG));
    }

    @Test
    void testRenameRootNetworkTagTakesOverAnEntryLeftUnderTheNewName() {
        List<ModificationInfos> modifications = networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(switchModification("v1d1")));
        UUID modificationUuid = modifications.getFirst().getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(modificationUuid), ROOT_NETWORK_TAG, false);
        // a leftover entry, the new tag naming no root network of the study yet
        networkModificationRepository.updateRootNetworkApplicability(List.of(modificationUuid), OTHER_ROOT_NETWORK_TAG, true);

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_3), ROOT_NETWORK_TAG, OTHER_ROOT_NETWORK_TAG);

        assertEquals(Map.of(OTHER_ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_3).get(modificationUuid),
                "On a modification the study owns the tag is really renamed: if an entry already exists under the new name, it is replaced");
    }

    /**
     * <pre>
     * composite                  (no entry)
     * ├── inner composite        (no entry)
     * │   ├── v1d1               false
     * │   ├── v1d2               true
     * │   └── v1d3               (no entry)
     * ├── v2d1                   false
     * ├── v2d2                   true
     * └── v2d3                   (no entry)
     * </pre>
     * Both levels hold the three states a tag can be in: set to false, set to true, and no entry at all.
     *
     * @return the composite holding that tree
     */
    private UUID compositeWithEveryApplicabilityCase() {
        UUID innerUuid = insertComposite(TEST_GROUP_ID_3, false, "v1d1", "v1d2", "v1d3");
        List<UUID> siblingUuids = networkModificationRepository.saveModifications(TEST_GROUP_ID_3,
                List.of(switchModification("v2d1"), switchModification("v2d2"), switchModification("v2d3")))
                .stream().map(ModificationInfos::getUuid).toList();
        UUID compositeUuid = networkModificationRepository.createNetworkCompositeModification(
                Stream.concat(Stream.of(innerUuid), siblingUuids.stream()).toList(), "source");

        List<UUID> contentUuids = networkModificationRepository.getBasicNetworkModificationsFromComposite(List.of(compositeUuid))
                .stream().map(ModificationInfos::getUuid).toList();
        List<UUID> innerUuids = networkModificationRepository.getBasicNetworkModificationsFromComposite(List.of(contentUuids.get(0)))
                .stream().map(ModificationInfos::getUuid).toList();
        networkModificationRepository.updateRootNetworkApplicability(List.of(innerUuids.get(0)), ROOT_NETWORK_TAG, false);
        networkModificationRepository.updateRootNetworkApplicability(List.of(innerUuids.get(1)), ROOT_NETWORK_TAG, true);
        networkModificationRepository.updateRootNetworkApplicability(List.of(contentUuids.get(1)), ROOT_NETWORK_TAG, false);
        networkModificationRepository.updateRootNetworkApplicability(List.of(contentUuids.get(2)), ROOT_NETWORK_TAG, true);

        assertEquals(List.of(Map.of(), Map.of(ROOT_NETWORK_TAG, false), Map.of(ROOT_NETWORK_TAG, true), Map.of(),
                        Map.of(ROOT_NETWORK_TAG, false), Map.of(ROOT_NETWORK_TAG, true), Map.of()),
                applicabilitiesInDepth(compositeUuid), "the tree the copy has to carry over");
        return compositeUuid;
    }

    @Test
    void testExportingACompositeKeepsTheApplicabilitiesOfItsWholeTree() {
        UUID sourceUuid = compositeWithEveryApplicabilityCase();

        UUID exportedUuid = networkModificationRepository.createNetworkCompositeModification(List.of(sourceUuid), "exported");

        assertEquals(applicabilitiesInDepth(sourceUuid), applicabilitiesInDepth(exportedUuid),
                "Exporting a composite carries the applicabilities of everything it holds, however deep");
    }

    @Test
    void testInsertingACompositeKeepsTheApplicabilitiesOfItsWholeTree() {
        UUID sourceUuid = compositeWithEveryApplicabilityCase();

        UUID insertedUuid = networkModificationRepository.insertCompositeModifications(TEST_GROUP_ID,
                List.of(new CompositeInfos(sourceUuid, "composite", false, "description"))).getFirst().getUuid();

        assertEquals(applicabilitiesInDepth(sourceUuid), applicabilitiesInDepth(insertedUuid),
                "Inserting a composite carries the applicabilities of everything it holds, however deep");
    }

    @Test
    void testReadingASharedModificationCarriesTheApplicabilitiesOfItsContent() {
        UUID sharedUuid = compositeWithEveryApplicabilityCase();

        UUID referenceUuid = networkModificationRepository.insertCompositeModifications(TEST_GROUP_ID_2,
                List.of(new CompositeInfos(sharedUuid, "composite", true, "description"))).getFirst().getUuid();

        ModificationReferenceInfos reference = (ModificationReferenceInfos) networkModificationRepository.getModificationInfo(referenceUuid);

        assertEquals(applicabilitiesInDepth(sharedUuid), applicabilitiesInDepth(reference.getReferenceInfos()),
                "Reading a shared modification carries the applicabilities of its whole tree, an empty one rather than a null where the tag says nothing");
    }

    /**
     * @return the applicabilities of everything a modification holds, depth first, the order the content is read in
     */
    private List<Map<String, Boolean>> applicabilitiesInDepth(UUID containerUuid) {
        return networkModificationRepository.getBasicNetworkModificationsFromComposite(List.of(containerUuid)).stream()
                .flatMap(content -> Stream.concat(Stream.of(content.getApplicabilityByRootNetworkTag()),
                        applicabilitiesInDepth(content.getUuid()).stream()))
                .toList();
    }

    /**
     * @return the same, but from the DTO instead of the database (references do not hold applicabilities)
     */
    private static List<Map<String, Boolean>> applicabilitiesInDepth(ModificationInfos modificationInfos) {
        if (!(modificationInfos instanceof CompositeModificationInfos composite)) {
            return List.of();
        }
        return composite.getModificationsInfos().stream()
                .flatMap(content -> Stream.concat(Stream.of(content.getApplicabilityByRootNetworkTag()),
                        applicabilitiesInDepth(content).stream()))
                .toList();
    }

    @Test
    void testRenameRootNetworkTagReachesNestedComposites() {
        // an inner composite, copied into a group, then wrapped with a sibling into an outer one
        UUID innerCompositeUuid = insertComposite(TEST_GROUP_ID_3, false, "v1d1");
        UUID siblingUuid = networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(switchModification("v1d2"))).getFirst().getUuid();
        UUID outerSourceUuid = networkModificationRepository.createNetworkCompositeModification(List.of(innerCompositeUuid, siblingUuid), "outer");
        UUID outerCompositeUuid = networkModificationRepository.insertCompositeModifications(TEST_GROUP_ID_2,
                List.of(new CompositeInfos(outerSourceUuid, "outer", false, "description"))).getFirst().getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(outerCompositeUuid), ROOT_NETWORK_TAG, false);

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_2), ROOT_NETWORK_TAG, RENAMED_ROOT_NETWORK_TAG);

        assertEquals(4, assertApplicabilityRenamedInDepth(outerCompositeUuid, getApplicabilities(TEST_GROUP_ID_2).get(outerCompositeUuid)),
                "the outer composite, the inner one and the sibling it holds, and the modification inside the inner one");
    }

    /**
     * Asserts that the composite and everything it holds, however deep, carry the renamed tag and only it.
     *
     * @return how many modifications were visited, the caller checking the descent did reach the whole tree
     */
    private int assertApplicabilityRenamedInDepth(UUID modificationUuid, Map<String, Boolean> applicabilityByRootNetworkTag) {
        assertEquals(Map.of(RENAMED_ROOT_NETWORK_TAG, false), applicabilityByRootNetworkTag);
        return 1 + getApplicabilitiesByModificationsInside(modificationUuid).entrySet().stream()
                .mapToInt(content -> assertApplicabilityRenamedInDepth(content.getKey(), content.getValue()))
                .sum();
    }

    @Test
    void testRenameRootNetworkTagReachesASharedModificationNestedInAnother() {
        // a shared composite, referenced from a group, then wrapped with a sibling into a composite shared in turn
        UUID innerReferenceUuid = insertComposite(TEST_GROUP_ID_3, true, "v1d1");
        UUID siblingUuid = networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(switchModification("v1d2"))).getFirst().getUuid();
        UUID outerSharedUuid = networkModificationRepository.createNetworkCompositeModification(List.of(innerReferenceUuid, siblingUuid), "outer");
        UUID outerReferenceUuid = networkModificationRepository.insertCompositeModifications(TEST_GROUP_ID_2,
                List.of(new CompositeInfos(outerSharedUuid, "outer", true, "description"))).getFirst().getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(outerReferenceUuid), ROOT_NETWORK_TAG, false);
        assertEquals(Map.of(ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_3).get(innerReferenceUuid),
                "The update reached the innermost shared modification, so the rename has to follow it there");

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_2), ROOT_NETWORK_TAG, RENAMED_ROOT_NETWORK_TAG);

        assertEquals(Map.of(ROOT_NETWORK_TAG, false, RENAMED_ROOT_NETWORK_TAG, false),
                getApplicabilities(TEST_GROUP_ID_3).get(innerReferenceUuid),
                "A shared modification referenced from inside another one carries the applicability too");
    }

    @Test
    void testRenameRootNetworkTagReusesAnEntryTheSharedModificationAlreadyHas() {
        UUID referenceUuid = insertComposite(TEST_GROUP_ID_2, true, "v1d1");
        UUID sharedUuid = ((ModificationReferenceInfos) networkModificationRepository.getModificationInfo(referenceUuid)).getReferenceId();
        networkModificationRepository.updateRootNetworkApplicability(List.of(sharedUuid), ROOT_NETWORK_TAG, false);
        // another group already named a root network of its own with the tag this one is being renamed to
        networkModificationRepository.updateRootNetworkApplicability(List.of(sharedUuid), RENAMED_ROOT_NETWORK_TAG, true);

        networkModificationRepository.renameRootNetworkTag(List.of(TEST_GROUP_ID_2), ROOT_NETWORK_TAG, RENAMED_ROOT_NETWORK_TAG);

        assertEquals(Map.of(ROOT_NETWORK_TAG, false, RENAMED_ROOT_NETWORK_TAG, true),
                getApplicabilities(TEST_GROUP_ID_2).get(referenceUuid),
                "A shared modification is never overwritten: the entry the other group set is reused as it is");
    }

    @Test
    void testDeleteRootNetworkTagsLeavesTheSharedModificationsAlone() {
        UUID referenceUuid = insertComposite(TEST_GROUP_ID_2, true, "v1d1");
        networkModificationRepository.updateRootNetworkApplicability(List.of(referenceUuid), ROOT_NETWORK_TAG, false);
        UUID ownedUuid = networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(switchModification("v1d2"))).getFirst().getUuid();
        networkModificationRepository.updateRootNetworkApplicability(List.of(ownedUuid), ROOT_NETWORK_TAG, false);
        networkModificationRepository.updateRootNetworkApplicability(List.of(ownedUuid), OTHER_ROOT_NETWORK_TAG, false);
        networkModificationRepository.updateRootNetworkApplicability(List.of(ownedUuid), RENAMED_ROOT_NETWORK_TAG, false);

        // deleting several root networks at once drops all their tags in one go
        networkModificationRepository.deleteRootNetworkTags(List.of(TEST_GROUP_ID_2, TEST_GROUP_ID_3), List.of(ROOT_NETWORK_TAG, OTHER_ROOT_NETWORK_TAG));

        assertEquals(Map.of(RENAMED_ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_3).get(ownedUuid),
                "The deleted root networks take their own applicabilities away, and only those");
        assertEquals(Map.of(ROOT_NETWORK_TAG, false), getApplicabilities(TEST_GROUP_ID_2).get(referenceUuid),
                "Another group may name a root network with that very tag, so a shared modification is never cleaned");
    }

    private List<UUID> activeCompositeContentUuids(UUID groupUuid, String rootNetworkTag) {
        CompositeModificationInfos composite = (CompositeModificationInfos) networkModificationRepository
                .getActiveModifications(groupUuid, rootNetworkTag).getFirst();
        return composite.getModificationsInfos().stream().map(ModificationInfos::getUuid).toList();
    }

    private List<UUID> activeModificationUuids(UUID groupUuid, String rootNetworkTag) {
        return networkModificationRepository.getActiveModifications(groupUuid, rootNetworkTag).stream()
                .map(ModificationInfos::getUuid).toList();
    }

    @Test
    void testGetStandaloneNetworkModificationReturnsExpected() {
        ModificationEntity modification = ModificationEntity.fromDTO(CreateCouplingDeviceInfos.builder()
                .couplingDeviceInfos(CouplingDeviceInfos.builder()
                        .busbarSectionId1("bbs1")
                        .busbarSectionId2("bbs2")
                        .build())
                .build());
        List<ModificationInfos> savedModificationInfos = networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modification));

        AbstractModification standaloneNetworkModification = networkModificationRepository.getStandaloneNetworkModification(savedModificationInfos.getFirst().getUuid());

        assertThat(standaloneNetworkModification).isEqualTo(savedModificationInfos.getFirst().toModification());
    }

    @Test
    void testGetNonExistentStandaloneNetworkModificationThrowsException() {
        UUID nonExistingUuid = UUID.randomUUID();

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getStandaloneNetworkModification(nonExistingUuid));
    }

    @Test
    void testGetStandaloneNetworkModificationsReturnsExpected() {
        ModificationEntity modificationOne = ModificationEntity.fromDTO(CreateCouplingDeviceInfos.builder()
                .couplingDeviceInfos(CouplingDeviceInfos.builder()
                        .busbarSectionId1("bbs1")
                        .busbarSectionId2("bbs2")
                        .build())
                .build());
        ModificationEntity modificationTwo = ModificationEntity.fromDTO(StaticVarCompensatorCreationInfos.builder()
                .equipmentId("idStaticVarCompensator1").equipmentName("nameStaticVarCompensator1")
                .voltageLevelId("vlId1")
                .busOrBusbarSectionId("busId1")
                .minSusceptance(200.0)
                .maxSusceptance(224.0)
                .regulationMode(VOLTAGE)
                .voltageSetpoint(200.0)
                .voltageRegulationType(DISTANT)
                .regulatingTerminalId("testTerminalId1")
                .regulatingTerminalType("STATIC_VAR_COMPENSATOR").regulatingTerminalVlId("idVlTest1")
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(1).build());
        List<ModificationInfos> savedModificationInfos = networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationOne, modificationTwo));
        Map<UUID, AbstractModification> expectedModifications = savedModificationInfos.stream()
                .collect(Collectors.toMap(ModificationInfos::getUuid, ModificationInfos::toModification));

        Map<UUID, AbstractModification> standaloneNetworkModifications = networkModificationRepository.getStandaloneNetworkModifications(List.of(savedModificationInfos.getFirst().getUuid(),
                savedModificationInfos.getLast().getUuid()), false);

        assertThat(standaloneNetworkModifications).isEqualTo(expectedModifications);
    }

    @Test
    void testGetStandaloneNetworkModificationsWithoutErrorOnMissingModificationReturnsExpected() {
        ModificationEntity modificationOne = ModificationEntity.fromDTO(CreateCouplingDeviceInfos.builder()
                .couplingDeviceInfos(CouplingDeviceInfos.builder()
                        .busbarSectionId1("bbs1")
                        .busbarSectionId2("bbs2")
                        .build())
                .build());

        List<ModificationInfos> savedModificationInfos = networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationOne));
        Map<UUID, AbstractModification> expectedModifications = savedModificationInfos.stream()
                .collect(Collectors.toMap(ModificationInfos::getUuid, ModificationInfos::toModification));

        Map<UUID, AbstractModification> standaloneNetworkModifications = networkModificationRepository.getStandaloneNetworkModifications(List.of(savedModificationInfos.getFirst().getUuid(),
                UUID.randomUUID()), false);

        assertThat(standaloneNetworkModifications).isEqualTo(expectedModifications);
    }

    @Test
    void testGetStandaloneNetworkModificationsWithErrorOnMissingModificationThrowsException() {
        List<UUID> nonExistingUuids = List.of(UUID.randomUUID(), UUID.randomUUID());

        assertThrows(NetworkModificationServerException.class, () -> networkModificationRepository.getStandaloneNetworkModifications(nonExistingUuids, true));
    }
}
