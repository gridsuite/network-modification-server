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
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LinesAttachToSplitLinesEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineSplitWithVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;


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

    public GroovyScriptModificationInfos getGroovyScriptModification(UUID modificationUuid) {
        return (GroovyScriptModificationInfos) networkModificationRepository.getModificationInfo(modificationUuid);
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

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(nullModifEntity, stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity));

        List<ModificationInfos> modificationEntities = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(6, modificationEntities.size());
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

        assertEquals(6, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of());
        assertEquals(6, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(stringModifEntity.getId(), boolModifEntity.getId()));
        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

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
        var createLoadEntity1 = LoadCreationInfos.builder().equipmentId("idLoad1").equipmentName("nameLoad1").loadType(LoadType.AUXILIARY).voltageLevelId("vlId1").busOrBusbarSectionId("busId1").activePower(100.).reactivePower(20.).connectionName("top1").connectionDirection(ConnectablePosition.Direction.TOP).build().toEntity();
        var createLoadEntity2 = LoadCreationInfos.builder().equipmentId("idLoad2").equipmentName("nameLoad2").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId2").busOrBusbarSectionId("busId2").activePower(80.).reactivePower(30.).connectionName("bottom1").connectionDirection(ConnectablePosition.Direction.BOTTOM).build().toEntity();
        var createLoadEntity3 = LoadCreationInfos.builder().equipmentId("idLoad3").equipmentName("nameLoad3").loadType(LoadType.FICTITIOUS).voltageLevelId("vlId3").busOrBusbarSectionId("busId3").activePower(50.).reactivePower(90.).connectionName("top2").connectionDirection(ConnectablePosition.Direction.TOP).build().toEntity();

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
        var createGeneratorEntity1 = networkModificationRepository.createGeneratorEntity("idGenerator1", "nameGenerator1", EnergySource.HYDRO, "vlId1", "busId1", 100.0, 800.0, 10., 500., 50., true, 225., 20., 30., 50., true, 8f, 37., 46., "testTerminalId1", "LINE", "idVlTest1", 25., false, List.of(), "Top", ConnectablePosition.Direction.TOP);
        var createGeneratorEntity2 = networkModificationRepository.createGeneratorEntity("idGenerator2", "nameGenerator2", EnergySource.SOLAR, "vlId2", "busId2", 0., 300., 5., 150., 30., false, 380.0, 30., 10., 20., false, null, 37., 46., null, null, "idVlTest2", 25., false, List.of(), "Bot", ConnectablePosition.Direction.BOTTOM);
        var createGeneratorEntity3 = networkModificationRepository.createGeneratorEntity("idGenerator3", "nameGenerator3", EnergySource.OTHER, "vlId3", "busId3", 10., 900., 5., 250., 20., true, 150.0, null, null, null, false, null, 37., null, "testTerminalId2", "BATTERY", "idVlTest3", 25., true,
                List.of(new ReactiveCapabilityCurveCreationEmbeddable(33., 44., 55.)), "Top", ConnectablePosition.Direction.TOP);

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
            .equipmentId("shunt1").equipmentName("nameOne")
            .currentNumberOfSections(1).maximumNumberOfSections(2)
            .susceptancePerSection(1.).isIdenticalSection(true)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();
        var shunt2 = ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt2").equipmentName("notNameOne")
            .currentNumberOfSections(1).maximumNumberOfSections(2)
            .susceptancePerSection(1.).isIdenticalSection(true)
            .voltageLevelId("vlId1").busOrBusbarSectionId("busId1")
            .build();

        var createShuntCompensatorEntity1 = networkModificationRepository.createShuntCompensatorEntity(shunt1);
        var createShuntCompensatorEntity2 = networkModificationRepository.createShuntCompensatorEntity(shunt2);

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
        var createLineEntity1 = networkModificationRepository.createLineEntity("idLine1", "nameLine1", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", null, null, "cn11", ConnectablePosition.Direction.TOP, "cn22", ConnectablePosition.Direction.TOP);
        var createLineEntity2 = networkModificationRepository.createLineEntity("idLine2", "nameLine2", 2.0, 2.2, 20.0, 22.0, 200.0, 200.2, "vlId21", "busId21", "vlId22", "busId22", null, 5.0, "cn33", ConnectablePosition.Direction.TOP, "cn44", ConnectablePosition.Direction.BOTTOM);
        var createLineEntity3 = networkModificationRepository.createLineEntity("idLine3", "nameLine3", 3.0, 3.3, 30.0, 33.0, 300.0, 300.3, "vlId31", "busId31", "vlId32", "busId32", 5.0, null, "cn55", ConnectablePosition.Direction.TOP, "cn66", ConnectablePosition.Direction.TOP);
        var createLineEntity4 = networkModificationRepository.createLineEntity("idLine4", "nameLine4", 3.0, 3.3, null, null, null, null, "vlId41", "busId41", "vlId42", "busId42", 5.0, 4.0, "cn77", ConnectablePosition.Direction.TOP, "cn88", ConnectablePosition.Direction.BOTTOM);

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
    public void testTwoWindingsTransformerCreation() {
        List<TapChangerStepCreationEmbeddable> phaseTapChangerStepCreationEmbeddables = new ArrayList<>();
        phaseTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 1, 1, 0, 0, 0, 0, 0.));
        phaseTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 2, 1, 0, 0, 0, 0, 0.));
        phaseTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 3, 1, 0, 0, 0, 0, 0.));
        var createTwoWindingsTransformerEntity1 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt1", "name2wt1", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, -1, "vlId11", "busId11", "vlId12", "busId12", null, null, "cn11", ConnectablePosition.Direction.TOP, "cn22", ConnectablePosition.Direction.TOP, 1, 2, false, null, null, null, null, PhaseTapChanger.RegulationMode.CURRENT_LIMITER, null, null, null, null, null, null, null, null, null, null, phaseTapChangerStepCreationEmbeddables);

        List<TapChangerStepCreationEmbeddable> ratioTapChangerStepCreationEmbeddables = new ArrayList<>();
        ratioTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 5, 1, 0, 0, 0, 0, null));
        ratioTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 6, 1, 0, 0, 0, 0, null));
        ratioTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 7, 1, 0, 0, 0, 0, null));
        ratioTapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 8, 1, 0, 0, 0, 0, null));
        var createTwoWindingsTransformerEntity2 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt2", "name2wt2", 2.0, 1.2, 11.0, 12.0, 101.0, 100.2, -1, "vlId11", "busId11", "vlId12", "busId12", 480.0, 480.0, "cn33", ConnectablePosition.Direction.TOP, "cn44", ConnectablePosition.Direction.BOTTOM, null, null, null, null, null, null, null, null, null,  5, 6, true, 1., "v2load", "v2", "LOAD", true, 50., ratioTapChangerStepCreationEmbeddables);
        var createTwoWindingsTransformerEntity3 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt3", "name2wt3", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, -1, "vlId11", "busId11", "vlId12", "busId12", 485.0, 480.0, "cn55", ConnectablePosition.Direction.TOP, "cn66", ConnectablePosition.Direction.TOP, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null);
        var createTwoWindingsTransformerEntity4 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt4", "name2wt4", 2.0, 1.2, 11.0, 12.0, 101.0, 100.2, -1, "vlId11", "busId11", "vlId12", "busId12", null, 490.0, "cn77", ConnectablePosition.Direction.TOP, "cn88", ConnectablePosition.Direction.BOTTOM, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createTwoWindingsTransformerEntity1, createTwoWindingsTransformerEntity2, createTwoWindingsTransformerEntity3, createTwoWindingsTransformerEntity4));
        assertRequestsCount(1, 21, 4, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(4, modificationInfos.size());

        assertThat(getTwoWindingsTransformerCreationModification(modificationInfos.get(0).getUuid()),
            MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity1).toModificationInfos()));
        assertThat(getTwoWindingsTransformerCreationModification(modificationInfos.get(1).getUuid()),
            MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity2).toModificationInfos()));
        assertThat(getTwoWindingsTransformerCreationModification(modificationInfos.get(2).getUuid()),
            MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity3).toModificationInfos()));
        assertThat(getTwoWindingsTransformerCreationModification(modificationInfos.get(3).getUuid()),
            MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity4).toModificationInfos()));
        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(createTwoWindingsTransformerEntity1.getId(), createTwoWindingsTransformerEntity2.getId()));
        assertRequestsCount(4, 0, 2, 8);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, true, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID, true);
        assertRequestsCount(5, 0, 0, 10);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true, true)
        );
    }

    @Test
    public void insertModificationTest() {
        var groovyScriptModificationEntity1 = networkModificationRepository.createGroovyScriptModificationEntity("script1");
        var groovyScriptModificationEntity2 = networkModificationRepository.createGroovyScriptModificationEntity("script2");
        var groovyScriptModificationEntity3 = networkModificationRepository.createGroovyScriptModificationEntity("script3");
        var groovyScriptModificationEntity4 = networkModificationRepository.createGroovyScriptModificationEntity("script4");
        var groovyScriptModificationEntity5 = networkModificationRepository.createGroovyScriptModificationEntity("script5");
        var groovyScriptModificationEntity6 = networkModificationRepository.createGroovyScriptModificationEntity("scriptSaucisse");

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity1, groovyScriptModificationEntity2,
            groovyScriptModificationEntity3, groovyScriptModificationEntity4, groovyScriptModificationEntity5, groovyScriptModificationEntity6));
        assertRequestsCount(1, 13, 6, 0);

        var modificationOriginal = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, TEST_GROUP_ID, List.of(groovyScriptModificationEntity6.getId()), groovyScriptModificationEntity2.getId());
        assertRequestsCount(2, 0, 6, 0);

        var modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        // [0:1, 1:6, 2:2, 3:3, 4:4 ,5:5 ]
        var expected = List.of(modificationOriginal.get(0), modificationOriginal.get(5),
            modificationOriginal.get(1), modificationOriginal.get(2), modificationOriginal.get(3), modificationOriginal.get(4));

        assertEquals(getIds(expected), getIds(modification));

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, TEST_GROUP_ID, List.of(groovyScriptModificationEntity3.getId(), groovyScriptModificationEntity6.getId()), null);
        assertRequestsCount(2, 0, 6, 0);

        // [0:1, 1:2, 2:4, 3:5, 4:6, 5:3 ]
        modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        expected = List.of(modificationOriginal.get(0), modificationOriginal.get(1), modificationOriginal.get(3),
            modificationOriginal.get(4), modificationOriginal.get(2), modificationOriginal.get(5));
        assertEquals(getIds(expected), getIds(modification));

    }

    @Test
    public void testMoveModificationsBetweenTwoGroups() {
        var groovyScriptModificationEntity1 = networkModificationRepository.createGroovyScriptModificationEntity("script1");
        var groovyScriptModificationEntity2 = networkModificationRepository.createGroovyScriptModificationEntity("script2");
        var groovyScriptModificationEntity3 = networkModificationRepository.createGroovyScriptModificationEntity("script3");
        var groovyScriptModificationEntity4 = networkModificationRepository.createGroovyScriptModificationEntity("script4");
        var groovyScriptModificationEntity5 = networkModificationRepository.createGroovyScriptModificationEntity("script5");
        var groovyScriptModificationEntity6 = networkModificationRepository.createGroovyScriptModificationEntity("script6");

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity1, groovyScriptModificationEntity2,
                groovyScriptModificationEntity3, groovyScriptModificationEntity4));
        assertRequestsCount(1, 9, 4, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptModificationEntity5, groovyScriptModificationEntity6));
        assertRequestsCount(1, 5, 2, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, List.of(groovyScriptModificationEntity2.getId(), groovyScriptModificationEntity3.getId()), null);
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
        var groovyScriptModificationEntity1 = networkModificationRepository.createGroovyScriptModificationEntity("script1");
        var groovyScriptModificationEntity2 = networkModificationRepository.createGroovyScriptModificationEntity("script2");
        var groovyScriptModificationEntity3 = networkModificationRepository.createGroovyScriptModificationEntity("script3");
        var groovyScriptModificationEntity4 = networkModificationRepository.createGroovyScriptModificationEntity("script4");
        var groovyScriptModificationEntity5 = networkModificationRepository.createGroovyScriptModificationEntity("script5");
        var groovyScriptModificationEntity6 = networkModificationRepository.createGroovyScriptModificationEntity("script6");

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity1, groovyScriptModificationEntity2,
                groovyScriptModificationEntity3, groovyScriptModificationEntity4));
        assertRequestsCount(1, 9, 4, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptModificationEntity5, groovyScriptModificationEntity6));
        assertRequestsCount(1, 5, 2, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, List.of(groovyScriptModificationEntity2.getId(), groovyScriptModificationEntity3.getId()), groovyScriptModificationEntity6.getId());
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
        var groovyScriptModificationEntity1 = networkModificationRepository.createGroovyScriptModificationEntity("script1");
        var groovyScriptModificationEntity2 = networkModificationRepository.createGroovyScriptModificationEntity("script2");
        var groovyScriptModificationEntity3 = networkModificationRepository.createGroovyScriptModificationEntity("script3");
        var groovyScriptModificationEntity4 = networkModificationRepository.createGroovyScriptModificationEntity("script4");
        var groovyScriptModificationEntity5 = networkModificationRepository.createGroovyScriptModificationEntity("script5");
        var groovyScriptModificationEntity6 = networkModificationRepository.createGroovyScriptModificationEntity("script6");

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity1, groovyScriptModificationEntity2));
        assertRequestsCount(1, 5, 2, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_2, List.of(groovyScriptModificationEntity3, groovyScriptModificationEntity4));
        assertRequestsCount(1, 5, 2, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.saveModifications(TEST_GROUP_ID_3, List.of(groovyScriptModificationEntity5, groovyScriptModificationEntity6));
        assertRequestsCount(1, 5, 2, 0);

        var modificationOriginal1 = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        var modificationOriginal2 = networkModificationRepository.getModifications(TEST_GROUP_ID_2, true, true);
        var modificationOriginal3 = networkModificationRepository.getModifications(TEST_GROUP_ID_3, true, true);

        // moving modifications from a wrong group should work but return their UUID in response
        SQLStatementCountValidator.reset();
        List<UUID> modificationsToMoveUuid = List.of(groovyScriptModificationEntity1.getId(), groovyScriptModificationEntity3.getId());
        assertEquals(List.of(groovyScriptModificationEntity3.getId()), networkModificationRepository.moveModifications(TEST_GROUP_ID_3, TEST_GROUP_ID, modificationsToMoveUuid, null));
        assertRequestsCount(4, 0, 5, 0);

        // moving modification with reference node not in destination group shoud throw exception
        SQLStatementCountValidator.reset();
        List <UUID> modificationsToMoveUuid2 = List.of(groovyScriptModificationEntity1.getId());
        UUID referenceNodeUuid = groovyScriptModificationEntity2.getId();
        assertThrows(NetworkModificationException.class, () -> networkModificationRepository.moveModifications(TEST_GROUP_ID_2, TEST_GROUP_ID, modificationsToMoveUuid2, referenceNodeUuid));
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
        var groovyScriptModificationEntity1 = networkModificationRepository.createGroovyScriptModificationEntity("script1");
        var groovyScriptModificationEntity2 = networkModificationRepository.createGroovyScriptModificationEntity("script2");
        var groovyScriptModificationEntity3 = networkModificationRepository.createGroovyScriptModificationEntity("script3");

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity1, groovyScriptModificationEntity2, groovyScriptModificationEntity3));
        assertRequestsCount(1, 7, 3, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getGroovyScriptModification(modificationInfos.get(0).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity1.toModificationInfos()));
        assertThat(getGroovyScriptModification(modificationInfos.get(1).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity2.toModificationInfos()));
        assertThat(getGroovyScriptModification(modificationInfos.get(2).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity3.toModificationInfos()));

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity2.getId(), groovyScriptModificationEntity3.getId()));
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
        var createSubstationEntity1 = networkModificationRepository.createSubstationEntity("idSubstation1", "nameSubstation1", Country.AR, null);
        var createSubstationEntity2 = networkModificationRepository.createSubstationEntity("idSubstation2", "nameSubstation2", Country.TD, null);
        var createSubstationEntity3 = networkModificationRepository.createSubstationEntity("idSubstation3", "nameSubstation3", Country.KG, null);

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
        List<BusbarSectionCreationEmbeddable> bbses;
        bbses = new ArrayList<>();
        Stream.iterate(1, n -> n + 1).limit(3 + 1).forEach(i -> bbses.add(new BusbarSectionCreationEmbeddable("bbs" + i, "NW", 1 + i, 1)));

        List<BusbarConnectionCreationEmbeddable> cnxes;
        cnxes = new ArrayList<>();
        Stream.iterate(0, n -> n + 1).limit(3).forEach(i -> {
            cnxes.add(new BusbarConnectionCreationEmbeddable("bbs.nw", "bbs.ne", SwitchKind.BREAKER));
            cnxes.add(new BusbarConnectionCreationEmbeddable("bbs.nw", "bbs.ne", SwitchKind.DISCONNECTOR));
        });

        VoltageLevelCreationEntity createVoltLvlEntity1 = networkModificationRepository.createVoltageLevelEntity("idVL1", "VLName", 379.0, "s1", bbses, cnxes);

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

        VoltageLevelCreationInfos createVoltLvlEntity1 = VoltageLevelCreationInfos.builder()
            .substationId("s1").nominalVoltage(379.0).equipmentId("idVL1").equipmentName("VLName")
            .busbarSections(bbses).busbarConnections(cnxes)
            .build();

        return createVoltLvlEntity1;
    }

    @Test
    public void testStatusLineModification() {
        List<BranchStatusModificationEntity> entities = List.of(
            networkModificationRepository.createBranchStatusModificationEntity("idLine1", BranchStatusModificationInfos.ActionType.LOCKOUT),
            networkModificationRepository.createBranchStatusModificationEntity("idLine2", BranchStatusModificationInfos.ActionType.TRIP),
            networkModificationRepository.createBranchStatusModificationEntity("idLine3", BranchStatusModificationInfos.ActionType.SWITCH_ON),
            networkModificationRepository.createBranchStatusModificationEntity("idLine4", BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE),
            networkModificationRepository.createBranchStatusModificationEntity("idLine5", BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO)
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
        LineSplitWithVoltageLevelEntity lineSplitEntity1 = LineSplitWithVoltageLevelEntity.toEntity(
                "lineId0", 30.0, null, "vl1", "bbsId", "line1id", "line1Name", "line2Id", "line2Name"
        );
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos(1, 0);
        LineSplitWithVoltageLevelEntity lineSplitEntity2 = LineSplitWithVoltageLevelEntity.toEntity(
                "lineId1", 30.0, voltageLevelCreationInfos, null, "bbsId", "line1id", "line1Name", "line2Id", "line2Name"
        );
        VoltageLevelCreationEntity voltageLevelCreationEntity = VoltageLevelCreationEntity.toEntity(voltageLevelCreationInfos);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lineSplitEntity1, voltageLevelCreationEntity, lineSplitEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(3, modificationInfos.size());

        assertThat(getLineSplitWithVoltageLevelModification(modificationInfos.get(0).getUuid()),
                MatcherLineSplitWithVoltageLevelInfos.createMatcherLineSplitWithVoltageLevelInfos(
                        lineSplitEntity1.toLineSplitWithVoltageLevelInfos()));

        assertThat(getLineSplitWithVoltageLevelModification(modificationInfos.get(2).getUuid()),
                MatcherLineSplitWithVoltageLevelInfos.createMatcherLineSplitWithVoltageLevelInfos(
                        lineSplitEntity2.toLineSplitWithVoltageLevelInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, List.of(lineSplitEntity1.getId(),
                voltageLevelCreationEntity.getId(),
                lineSplitEntity2.getId()));
        assertRequestsCount(3, 0, 0, 12);

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

        LineAttachToVoltageLevelEntity lineAttachToEntity1 = LineAttachToVoltageLevelEntity.toEntity(
                "lineId0", 40.0, "AttachmentPointId", null, null, "vl1", "bbsId", attachmentLine, "line1Id", "line1Name", "line2Id", "line2Name"
        );
        VoltageLevelCreationInfos voltageLevelCreationInfos = makeAVoltageLevelInfos(1, 0);
        LineAttachToVoltageLevelEntity lineAttachToEntity2 = LineAttachToVoltageLevelEntity.toEntity(
                "lineId1", 40.0, "AttachmentPointId", null, voltageLevelCreationInfos, null, "bbsId", attachmentLine, "line1Id", "line1Name", "line2Id", "line2Name"
        );
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(lineAttachToEntity1, lineAttachToEntity2));

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, modificationInfos.size());

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(0).getUuid()),
                MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos(
                        lineAttachToEntity1.toLineAttachToVoltageLevelInfos()));

        assertThat(getLineAttachToVoltageLevelModification(modificationInfos.get(1).getUuid()),
                MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos(
                        lineAttachToEntity2.toLineAttachToVoltageLevelInfos()));

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
        LinesAttachToSplitLinesEntity linesAttachToEntity1 = LinesAttachToSplitLinesEntity.toEntity(
                "lineId0", "lineId1", "lineId3", "vl1", "bbsId", "line1Id", "line1Name", "line2Id", "line2Name"
        );
        LinesAttachToSplitLinesEntity linesAttachToEntity2 = LinesAttachToSplitLinesEntity.toEntity(
                "lineId4", "lineId5", "lineId6", "vl2", "bbsId2", "line3Id", "line3Name", "line4Id", "line4Name"
        );
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

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
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
