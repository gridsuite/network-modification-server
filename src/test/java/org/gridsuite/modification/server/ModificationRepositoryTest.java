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
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

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

    @Autowired
    private ModificationGroupRepository modificationGroupRepository;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Before
    public void setUp() {
        modificationRepository.deleteAll();

        SQLStatementCountValidator.reset();
    }

    @Test
    public void test() {
        assertEquals(List.of(), this.modificationRepository.getModificationGroupsUuids());
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, true)
        );

        var nullModifEntity = modificationRepository.createEquipmentAttributeModification("id0", "attribute", null);
        var stringModifEntity = modificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        var boolModifEntity = modificationRepository.createEquipmentAttributeModification("id2", "attribute", true);
        var intModifEntity = modificationRepository.createEquipmentAttributeModification("id3", "attribute", 1);
        var floatModifEntity = modificationRepository.createEquipmentAttributeModification("id4", "attribute", 2F);
        var doubleModifEntity = modificationRepository.createEquipmentAttributeModification("id5", "attribute", 3D);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(nullModifEntity, stringModifEntity, boolModifEntity, intModifEntity, floatModifEntity, doubleModifEntity));

        List<ModificationInfos> modificationEntities = modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(6, modificationEntities.size());
        // Order is also checked
        assertThat(modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(0).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(nullModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(1).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(stringModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(2).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(boolModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(3).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(intModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(4).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(floatModifEntity.toEquipmentAttributeModificationInfos()));
        assertThat(modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modificationEntities.get(5).getUuid()),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(doubleModifEntity.toEquipmentAttributeModificationInfos()));

        assertEquals(6, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of());
        assertEquals(6, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(stringModifEntity.getId(), boolModifEntity.getId()));
        assertEquals(4, modificationRepository.getModifications(TEST_GROUP_ID, true).size());

        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, true)
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
        var modifEntity1 = modificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        var modifEntity2 = modificationRepository.createEquipmentAttributeModification("id2", "attribute", "foo");
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        assertRequestsCount(1, 5, 0, 0);
    }

    @Test
    public void testGetModificationQueryCount() {
        var modifEntity1 = modificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        var modifEntity2 = modificationRepository.createEquipmentAttributeModification("id2", "attribute", "foo");
        var modifEntity3 = modificationRepository.createEquipmentAttributeModification("id3", "attribute", "foo");
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2, modifEntity3));

        SQLStatementCountValidator.reset();
        modificationRepository.getModificationGroupsUuids();
        assertRequestsCount(1, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, modifEntity1.getId());
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getEquipmentAttributeModification(TEST_GROUP_ID, TEST_GROUP_ID)
        );
    }

    @Test
    public void testDeleteModificationQueryCount() {
        EquipmentAttributeModificationEntity<String> modifEntity1 = modificationRepository.createEquipmentAttributeModification("id1", "attribute", "foo");
        EquipmentAttributeModificationEntity<String> modifEntity2 = modificationRepository.createEquipmentAttributeModification("id2", "attribute", "foo");
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modifEntity1, modifEntity2));

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(modifEntity1.getId()));
        assertRequestsCount(1, 0, 0, 2);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(modifEntity2.getId()));
        assertRequestsCount(1, 0, 0, 0);

        // Non-existent group modification uuid
        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.deleteModificationGroup(TEST_GROUP_ID)
        );
    }

    @Test
    public void testLoadCreation() {
        var createLoadEntity1 = modificationRepository.createLoadEntity("idLoad1", "nameLoad1", LoadType.AUXILIARY, "vlId1", "busId1", 100.0, 20.0);
        var createLoadEntity2 = modificationRepository.createLoadEntity("idLoad2", "nameLoad2", LoadType.FICTITIOUS, "vlId2", "busId2", 80.0, 30.0);
        var createLoadEntity3 = modificationRepository.createLoadEntity("idLoad3", "nameLoad3", LoadType.UNDEFINED, "vlId3", "busId3", 50.0, 90.0);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLoadEntity1, createLoadEntity2, createLoadEntity3));
        assertRequestsCount(1, 7, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(3, modificationInfos.size());

        assertThat(modificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((LoadCreationEntity) createLoadEntity1).toModificationInfos()));
        assertThat(modificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((LoadCreationEntity) createLoadEntity2).toModificationInfos()));
        assertThat(modificationRepository.getLoadCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherLoadCreationInfos.createMatcherLoadCreationInfos(((LoadCreationEntity) createLoadEntity3).toModificationInfos()));

        assertEquals(3, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createLoadEntity2.getId(), createLoadEntity3.getId()));
        assertRequestsCount(1, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testGeneratorCreation() {
        var createGeneratorEntity1 = modificationRepository.createGeneratorEntity("idGenerator1", "nameGenerator1", EnergySource.HYDRO, "vlId1", "busId1", 100.0, 800.0, 10., 500., 50., true, 225.);
        var createGeneratorEntity2 = modificationRepository.createGeneratorEntity("idGenerator2", "nameGenerator2", EnergySource.SOLAR, "vlId2", "busId2", 0., 300., 5., 150., 30., false, 380.0);
        var createGeneratorEntity3 = modificationRepository.createGeneratorEntity("idGenerator3", "nameGenerator3", EnergySource.OTHER, "vlId3", "busId3", 10., 900., 5., 250., 20., true, 150.0);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(createGeneratorEntity1, createGeneratorEntity2, createGeneratorEntity3));
        assertRequestsCount(1, 7, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(3, modificationInfos.size());

        assertThat(modificationRepository.getGeneratorCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity1).toModificationInfos()));
        assertThat(modificationRepository.getGeneratorCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity2).toModificationInfos()));
        assertThat(modificationRepository.getGeneratorCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(((GeneratorCreationEntity) createGeneratorEntity3).toModificationInfos()));

        assertEquals(3, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createGeneratorEntity2.getId(), createGeneratorEntity3.getId()));
        assertRequestsCount(1, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testLineCreation() {
        var createLineEntity1 = modificationRepository.createLineEntity("idLine1", "nameLine1", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", null, null);
        var createLineEntity2 = modificationRepository.createLineEntity("idLine2", "nameLine2", 2.0, 2.2, 20.0, 22.0, 200.0, 200.2, "vlId21", "busId21", "vlId22", "busId22", null, 5.0);
        var createLineEntity3 = modificationRepository.createLineEntity("idLine3", "nameLine3", 3.0, 3.3, 30.0, 33.0, 300.0, 300.3, "vlId31", "busId31", "vlId32", "busId32", 5.0, null);
        var createLineEntity4 = modificationRepository.createLineEntity("idLine4", "nameLine4", 3.0, 3.3, null, null, null, null, "vlId41", "busId41", "vlId42", "busId42", 5.0, 4.0);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(createLineEntity1, createLineEntity2, createLineEntity3, createLineEntity4));
        assertRequestsCount(1, 13, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(4, modificationInfos.size());

        assertThat(modificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity1).toModificationInfos()));
        assertThat(modificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity2).toModificationInfos()));
        assertThat(modificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity3).toModificationInfos()));
        assertThat(modificationRepository.getLineCreationModification(TEST_GROUP_ID, modificationInfos.get(3).getUuid()),
            MatcherLineCreationInfos.createMatcherLineCreationInfos(((LineCreationEntity) createLineEntity4).toModificationInfos()));

        assertEquals(4, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createLineEntity2.getId(), createLineEntity3.getId()));
        assertRequestsCount(3, 0, 0, 6);

        SQLStatementCountValidator.reset();
        assertEquals(2, modificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertRequestsCount(4, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(4, 0, 0, 7);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testTwoWindingsTransformerCreation() {
        var createTwoWindingsTransformerEntity1 = modificationRepository.createTwoWindingsTransformerEntity("id2wt1", "name2wt1", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", null, null);
        var createTwoWindingsTransformerEntity2 = modificationRepository.createTwoWindingsTransformerEntity("id2wt2", "name2wt2", 2.0, 1.2, 11.0, 12.0, 101.0, 100.2, "vlId11", "busId11", "vlId12", "busId12", 480.0, 480.0);
        var createTwoWindingsTransformerEntity3 = modificationRepository.createTwoWindingsTransformerEntity("id2wt3", "name2wt3", 1.0, 1.1, 10.0, 11.0, 100.0, 100.1, "vlId11", "busId11", "vlId12", "busId12", 485.0, 480.0);
        var createTwoWindingsTransformerEntity4 = modificationRepository.createTwoWindingsTransformerEntity("id2wt4", "name2wt4", 2.0, 1.2, 11.0, 12.0, 101.0, 100.2, "vlId11", "busId11", "vlId12", "busId12", null, 490.0);
        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(createTwoWindingsTransformerEntity1, createTwoWindingsTransformerEntity2, createTwoWindingsTransformerEntity3, createTwoWindingsTransformerEntity4));
        assertRequestsCount(1, 14, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(4, modificationInfos.size());

        assertThat(modificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity1).toModificationInfos()));
        assertThat(modificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity2).toModificationInfos()));
        assertThat(modificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity3).toModificationInfos()));
        assertThat(modificationRepository.getTwoWindingsTransformerCreationModification(TEST_GROUP_ID, modificationInfos.get(3).getUuid()),
                MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(((TwoWindingsTransformerCreationEntity) createTwoWindingsTransformerEntity4).toModificationInfos()));
        assertEquals(4, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createTwoWindingsTransformerEntity1.getId(), createTwoWindingsTransformerEntity2.getId()));
        assertRequestsCount(3, 0, 0, 6);

        SQLStatementCountValidator.reset();
        assertEquals(2, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(5, 0, 0, 8);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
                NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, true)
        );
    }

    @Test
    public void testGroovyScript() {
        var groovyScriptModificationEntity1 = modificationRepository.createGroovyScriptModificationEntity("script1");
        var groovyScriptModificationEntity2 = modificationRepository.createGroovyScriptModificationEntity("script2");
        var groovyScriptModificationEntity3 = modificationRepository.createGroovyScriptModificationEntity("script3");

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(groovyScriptModificationEntity1, groovyScriptModificationEntity2, groovyScriptModificationEntity3));
        assertRequestsCount(1, 7, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID, false);
        assertEquals(3, modificationInfos.size());

        assertThat(modificationRepository.getGroovyScriptModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity1.toModificationInfos()));
        assertThat(modificationRepository.getGroovyScriptModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity2.toModificationInfos()));
        assertThat(modificationRepository.getGroovyScriptModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherGroovyScriptModificationInfos.createMatcherGroovyScriptModificationInfos(groovyScriptModificationEntity3.toModificationInfos()));

        assertEquals(3, modificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(groovyScriptModificationEntity2.getId(), groovyScriptModificationEntity3.getId()));
        assertRequestsCount(1, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, false)
        );
    }

    @Test
    public void testSubstationCreation() {
        var createSubstationEntity1 = modificationRepository.createSubstationEntity("idSubstation1", "nameSubstation1", Country.AR);
        var createSubstationEntity2 = modificationRepository.createSubstationEntity("idSubstation2", "nameSubstation2", Country.TD);
        var createSubstationEntity3 = modificationRepository.createSubstationEntity("idSubstation3", "nameSubstation3", Country.KG);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(createSubstationEntity1, createSubstationEntity2, createSubstationEntity3));
        assertRequestsCount(1, 7, 0, 0);

        List<ModificationInfos> modificationInfos = modificationRepository.getModifications(TEST_GROUP_ID, false);
        assertEquals(3, modificationInfos.size());

        assertThat(modificationRepository.getSubstationCreationModification(TEST_GROUP_ID, modificationInfos.get(0).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity1).toSubstationCreationInfos()));
        assertThat(modificationRepository.getSubstationCreationModification(TEST_GROUP_ID, modificationInfos.get(1).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity2).toSubstationCreationInfos()));
        assertThat(modificationRepository.getSubstationCreationModification(TEST_GROUP_ID, modificationInfos.get(2).getUuid()),
            MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(((SubstationCreationEntity) createSubstationEntity3).toSubstationCreationInfos()));

        assertEquals(3, modificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertEquals(List.of(TEST_GROUP_ID), this.modificationRepository.getModificationGroupsUuids());

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModifications(TEST_GROUP_ID, Set.of(createSubstationEntity2.getId(), createSubstationEntity3.getId()));
        assertRequestsCount(1, 0, 0, 4);

        SQLStatementCountValidator.reset();
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, false).size());
        assertRequestsCount(2, 0, 0, 0);

        SQLStatementCountValidator.reset();
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 3);

        assertThrows(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage(),
            NetworkModificationException.class, () -> modificationRepository.getModifications(TEST_GROUP_ID, false)
        );
    }

    @Test
    public void testStatusLineModification() {
        List<BranchStatusModificationEntity> entities = List.of(
            modificationRepository.createBranchStatusModificationEntity("idLine1", BranchStatusModificationInfos.ActionType.LOCKOUT),
            modificationRepository.createBranchStatusModificationEntity("idLine2", BranchStatusModificationInfos.ActionType.TRIP),
            modificationRepository.createBranchStatusModificationEntity("idLine3", BranchStatusModificationInfos.ActionType.SWITCH_ON),
            modificationRepository.createBranchStatusModificationEntity("idLine4", BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE),
            modificationRepository.createBranchStatusModificationEntity("idLine5", BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO)
        );

        modificationRepository.saveModifications(TEST_GROUP_ID, entities);
        assertRequestsCount(1, 11, 0, 0);

        List<BranchStatusModificationInfos> modificationInfos = modificationRepository.getModifications(
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
        modificationRepository.deleteModificationGroup(TEST_GROUP_ID);
        assertRequestsCount(2, 0, 0, 11);
    }

    private void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }
}
