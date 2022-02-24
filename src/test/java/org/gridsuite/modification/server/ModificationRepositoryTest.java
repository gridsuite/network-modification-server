/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.SwitchKind;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.*;
import org.jetbrains.annotations.NotNull;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
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

    @Test
    public void test() {
        assertEquals(List.of(), this.networkModificationRepository.getModificationGroupsUuids());
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
        );

        var nullModifEntity = networkModificationRepository.createEquipmentAttributeModification("id0", "attribute", null);
        var stringModifEntity = networkModificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        var boolModifEntity = networkModificationRepository.createEquipmentAttributeModification("id2", "attribute", true);
        var intModifEntity = networkModificationRepository.createEquipmentAttributeModification("id3", "attribute", 1);
        var floatModifEntity = networkModificationRepository.createEquipmentAttributeModification("id4", "attribute", 2F);
        var doubleModifEntity = networkModificationRepository.createEquipmentAttributeModification("id5", "attribute", 3D);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(nullModifEntity, stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity));

        List<ModificationInfos> modificationEntities = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(6, modificationEntities.size());
        // Order is also checked
        assertThat(networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(0).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(nullModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(1).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(stringModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(2).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(boolModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(3).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(intModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(4).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(floatModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(5).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(doubleModifEntity.toEquipmentAttributeModificationInfos()));

        assertEquals(6, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of());
        assertEquals(6, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(stringModifEntity.getId(), boolModifEntity.getId()));
        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());

        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertEquals(0, modificationRepository.findAll().size());
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
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
        var modifEntity1 = networkModificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        var modifEntity2 = networkModificationRepository.createEquipmentAttributeModification("id2", "attribute", "foo");
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        assertRequestsCount(1, 5, 2, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        var modifEntity1 = networkModificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        var modifEntity2 = networkModificationRepository.createEquipmentAttributeModification("id2", "attribute", "foo");
        var modifEntity3 = networkModificationRepository.createEquipmentAttributeModification("id3", "attribute", "foo");
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2, modifEntity3));

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modifEntity1.getId());
        assertRequestsCount(2, 0, 0, 0);

        // Non-existent modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, TEST_GROUP_ID)
        );
    }

    @Test
    public void testDeleteModificationQueryCount() {
        EquipmentAttributeModificationEntity<String> modifEntity1 = networkModificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        EquipmentAttributeModificationEntity<String> modifEntity2 = networkModificationRepository.createEquipmentAttributeModification("id2", "attribute", "foo");
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(modifEntity1.getId()));
        assertRequestsCount(2, 0, 0, 2);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        // Non-existent group modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID)
        );
    }

    @Test
    public void testLoadCreation() {
        var createLoadEntity1 = networkModificationRepository.createLoadEntity("idLoad1", "nameLoad1", LoadType.AUXILIARY, "vlId1", "busId1", 100.0, 20.0);
        var createLoadEntity2 = networkModificationRepository.createLoadEntity("idLoad2", "nameLoad2", LoadType.FICTITIOUS, "vlId2", "busId2", 80.0, 30.0);
        var createLoadEntity3 = networkModificationRepository.createLoadEntity("idLoad3", "nameLoad3", LoadType.UNDEFINED, "vlId3", "busId3", 50.0, 90.0);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLoadEntity1, createLoadEntity2, createLoadEntity3));
        assertRequestsCount(1, 7, 3, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(3, modificationInfos.size());

        assertThat(networkModificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((LoadCreationEntity) createLoadEntity1).toModificationInfos()));
        assertThat(networkModificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((LoadCreationEntity) createLoadEntity2).toModificationInfos()));
        assertThat(networkModificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((LoadCreationEntity) createLoadEntity3).toModificationInfos()));

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createLoadEntity2.getId(), createLoadEntity3.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testGeneratorCreation() {
        var createGeneratorEntity1 = networkModificationRepository.createGeneratorEntity("idGenerator1", "nameGenerator1", EnergySource.HYDRO, "vlId1", "busId1", 100.0, 800.0, 10., 500., 50., true, 225.);
        var createGeneratorEntity2 = networkModificationRepository.createGeneratorEntity("idGenerator2", "nameGenerator2", EnergySource.SOLAR, "vlId2", "busId2", 0., 300., 5., 150., 30., false, 380.0);
        var createGeneratorEntity3 = networkModificationRepository.createGeneratorEntity("idGenerator3", "nameGenerator3", EnergySource.OTHER, "vlId3", "busId3", 10., 900., 5., 250., 20., true, 150.0);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createGeneratorEntity1, createGeneratorEntity2, createGeneratorEntity3));
        assertRequestsCount(1, 7, 3, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(3, modificationInfos.size());

        assertThat(networkModificationRepository.getGeneratorCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity1).toModificationInfos()));
        assertThat(networkModificationRepository.getGeneratorCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity2).toModificationInfos()));
        assertThat(networkModificationRepository.getGeneratorCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity3).toModificationInfos()));

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createGeneratorEntity2.getId(), createGeneratorEntity3.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
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

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(2, modificationInfos.size());

        assertThat(networkModificationRepository.getShuntCompensatorCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherShuntCompensatorCreationInfos.createMatcher(createShuntCompensatorEntity1.toModificationInfos()));
        assertThat(networkModificationRepository.getShuntCompensatorCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherShuntCompensatorCreationInfos.createMatcher(createShuntCompensatorEntity2.toModificationInfos()));

        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createShuntCompensatorEntity2.getId()));
        assertRequestsCount(2, 0, 0, 2);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testLineCreation() {
        var createLineEntity1 = networkModificationRepository.createLineEntity("idLine1", "nameLine1", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", null, null);
        var createLineEntity2 = networkModificationRepository.createLineEntity("idLine2", "nameLine2", 2.0, 2.2, 20.0, 22.0, 200.0, 200.2, "vlId21", "busId21", "vlId22", "busId22", null, 5.0);
        var createLineEntity3 = networkModificationRepository.createLineEntity("idLine3", "nameLine3", 3.0, 3.3, 30.0, 33.0, 300.0, 300.3, "vlId31", "busId31", "vlId32", "busId32", 5.0, null);
        var createLineEntity4 = networkModificationRepository.createLineEntity("idLine4", "nameLine4", 3.0, 3.3, null, null, null, null, "vlId41", "busId41", "vlId42", "busId42", 5.0, 4.0);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLineEntity1, createLineEntity2, createLineEntity3, createLineEntity4));
        assertRequestsCount(1, 13, 4, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(4, modificationInfos.size());

        assertThat(networkModificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity1).toModificationInfos()));
        assertThat(networkModificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity2).toModificationInfos()));
        assertThat(networkModificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity3).toModificationInfos()));
        assertThat(networkModificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(3).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity4).toModificationInfos()));

        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createLineEntity2.getId(), createLineEntity3.getId()));
        assertRequestsCount(4, 0, 0, 6);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertRequestsCount(4, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(4, 0, 0, 7);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testTwoWindingsTransformerCreation() {
        var createTwoWindingsTransformerEntity1 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt1", "name2wt1", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", null, null);
        var createTwoWindingsTransformerEntity2 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt2", "name2wt2", 2.0, 1.2, 11.0, 12.0, 101.0, 100.2, "vlId11", "busId11", "vlId12", "busId12", 480.0, 480.0);
        var createTwoWindingsTransformerEntity3 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt3", "name2wt3", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", 485.0, 480.0);
        var createTwoWindingsTransformerEntity4 = networkModificationRepository.createTwoWindingsTransformerEntity("id2wt4", "name2wt4", 2.0, 1.2, 11.0, 12.0, 101.0, 100.2, "vlId11", "busId11", "vlId12", "busId12", null, 490.0);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createTwoWindingsTransformerEntity1, createTwoWindingsTransformerEntity2, createTwoWindingsTransformerEntity3, createTwoWindingsTransformerEntity4));
        assertRequestsCount(1, 14, 4, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(4, modificationInfos.size());

        assertThat(networkModificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity1).toModificationInfos()));
        assertThat(networkModificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity2).toModificationInfos()));
        assertThat(networkModificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity3).toModificationInfos()));
        assertThat(networkModificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(3).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity4).toModificationInfos()));
        assertEquals(4, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createTwoWindingsTransformerEntity1.getId(), createTwoWindingsTransformerEntity2.getId()));
        assertRequestsCount(4, 0, 0, 6);

        SQLStatementCountValidator.reset();
        assertEquals(2, networkModificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(5, 0, 0, 8);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, true)
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

        var modificationOriginal = networkModificationRepository.getModifications(TEST_GROUP_ID, true);

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity6.getId()), groovyScriptModificationEntity2.getId());
        assertRequestsCount(2, 0, 6, 0);

        var modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        // [0:1, 1:6, 2:2, 3:3, 4:4 ,5:5 ]
        var expected = List.of(modificationOriginal.get(0), modificationOriginal.get(5),
            modificationOriginal.get(1), modificationOriginal.get(2), modificationOriginal.get(3), modificationOriginal.get(4));

        assertEquals(getIds(expected), getIds(modification));

        SQLStatementCountValidator.reset();
        networkModificationRepository.moveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity3.getId(), groovyScriptModificationEntity6.getId()), null);
        assertRequestsCount(2, 0, 6, 0);

        // [0:1, 1:2, 2:4, 3:5, 4:6, 5:3 ]
        modification = networkModificationRepository.getModifications(TEST_GROUP_ID, true);
        expected = List.of(modificationOriginal.get(0), modificationOriginal.get(1), modificationOriginal.get(3),
            modificationOriginal.get(4), modificationOriginal.get(2), modificationOriginal.get(5));
        assertEquals(getIds(expected), getIds(modification));

    }

    @NotNull
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

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false);
        assertEquals(3, modificationInfos.size());

        assertThat(networkModificationRepository.getGroovyScriptModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity1.toModificationInfos()));
        assertThat(networkModificationRepository.getGroovyScriptModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity2.toModificationInfos()));
        assertThat(networkModificationRepository.getGroovyScriptModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity3.toModificationInfos()));

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(groovyScriptModificationEntity2.getId(), groovyScriptModificationEntity3.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false)
        );
    }

    @Test
    public void testSubstationCreation() {
        var createSubstationEntity1 = networkModificationRepository.createSubstationEntity("idSubstation1", "nameSubstation1", Country.AR);
        var createSubstationEntity2 = networkModificationRepository.createSubstationEntity("idSubstation2", "nameSubstation2", Country.TD);
        var createSubstationEntity3 = networkModificationRepository.createSubstationEntity("idSubstation3", "nameSubstation3", Country.KG);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createSubstationEntity1, createSubstationEntity2, createSubstationEntity3));
        assertRequestsCount(1, 7, 3, 0);

        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false);
        assertEquals(3, modificationInfos.size());

        assertThat(networkModificationRepository.getSubstationCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity1).toSubstationCreationInfos()));
        assertThat(networkModificationRepository.getSubstationCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity2).toSubstationCreationInfos()));
        assertThat(networkModificationRepository.getSubstationCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity3).toSubstationCreationInfos()));

        assertEquals(3, networkModificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertEquals(List.of(TEST_GROUP_ID), this.networkModificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createSubstationEntity2.getId(), createSubstationEntity3.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, networkModificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false)
        );
    }

    @Test
    public void testVoltageLevelCreation() {
        List<BusbarSectionCreationEmbeddable> bbses = new ArrayList<>();
        bbses.add(new BusbarSectionCreationEmbeddable("bbs.nw", "NW", 1, 1));
        bbses.add(new BusbarSectionCreationEmbeddable("bbs.ne", "NE", 1, 2));
        bbses.add(new BusbarSectionCreationEmbeddable("bbs.sw", "SW", 2, 1));

        List<BusbarConnectionCreationEmbeddable> cnxes = new ArrayList<>();
        cnxes.add(new BusbarConnectionCreationEmbeddable("bbs.nw", "bbs.ne", SwitchKind.BREAKER));
        cnxes.add(new BusbarConnectionCreationEmbeddable("bbs.nw", "bbs.sw", SwitchKind.DISCONNECTOR));
        cnxes.add(new BusbarConnectionCreationEmbeddable("bbs.ne", "bbs.ne", SwitchKind.DISCONNECTOR));

        EquipmentCreationEntity createVoltLvlEntity1 = networkModificationRepository.createVoltageLevelEntity("idVL1", "VLName", 379.0, "s1", bbses, cnxes);

        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(createVoltLvlEntity1));
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModifications(TEST_GROUP_ID, false);
        assertEquals(1, modificationInfos.size());

        assertThat(networkModificationRepository.getVoltageLevelCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherVoltageLevelCreationInfos.createMatcherVoltageLevelCreationInfos(((VoltageLevelCreationEntity) createVoltLvlEntity1).toVoltageLevelCreationInfos()));

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createVoltLvlEntity1.getId()));
        assertRequestsCount(2, 0, 0, 4);

        SQLStatementCountValidator.reset();
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 1);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> networkModificationRepository.getModifications(TEST_GROUP_ID, false)
        );
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
        networkModificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 11);
    }

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }
}
