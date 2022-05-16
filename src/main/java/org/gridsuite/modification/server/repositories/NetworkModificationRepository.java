/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.GroovyScriptModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Repository
public class NetworkModificationRepository {
    private final ModificationGroupRepository modificationGroupRepository;

    private final ModificationRepository modificationRepository;

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository, ModificationRepository modificationRepository) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteAll() {
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
    }

    public <T> EquipmentAttributeModificationEntity<T> createEquipmentAttributeModification(String equipmentId, String attributeName, T attributeValue) {
        EquipmentAttributeModificationEntity<?> modification;
        if (attributeValue == null) {
            modification = new StringEquipmentAttributeModificationEntity(equipmentId, attributeName, null);
        } else if (attributeValue.getClass().isEnum()) {
            modification = new StringEquipmentAttributeModificationEntity(equipmentId, attributeName, attributeValue.toString());
        } else {
            switch (attributeValue.getClass().getSimpleName()) {
                case "String":
                    modification = new StringEquipmentAttributeModificationEntity(equipmentId, attributeName, (String) attributeValue);
                    break;
                case "Boolean":
                    modification = new BooleanEquipmentAttributeModificationEntity(equipmentId, attributeName, (boolean) attributeValue);
                    break;
                case "Integer":
                    modification = new IntegerEquipmentAttributeModificationEntity(equipmentId, attributeName, (int) attributeValue);
                    break;
                case "Float":
                    modification = new FloatEquipmentAttributeModificationEntity(equipmentId, attributeName, (float) attributeValue);
                    break;
                case "Double":
                    modification = new DoubleEquipmentAttributeModificationEntity(equipmentId, attributeName, (double) attributeValue);
                    break;
                default:
                    throw new PowsyblException("Value type invalid : " + attributeValue.getClass().getSimpleName());
            }
        }

        return (EquipmentAttributeModificationEntity<T>) modification;
    }

    @Transactional // To have all create in the same transaction (atomic)
    public void saveModifications(UUID groupUuid, List<? extends ModificationEntity> modifications) {
        var modificationGroupEntity = this.modificationGroupRepository
                .findById(groupUuid)
                .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        modifications.forEach(modificationGroupEntity::addModification);
    }

    @Transactional // To have all move in the same transaction (atomic)
    public void moveModifications(UUID groupUuid, List<UUID> modifications, UUID before) {
        /* when before == null we move at the end of list */
        var modificationGroupEntity = getModificationGroup(groupUuid);

        Map<UUID, ModificationEntity> originalModifications = modificationRepository.findAllBaseByGroupId(groupUuid).stream()
            .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

        if (!originalModifications.keySet().containsAll(modifications) || (before != null && !originalModifications.containsKey(before))) {
            throw new NetworkModificationException(MODIFICATION_NOT_FOUND);
        }

        List<ModificationEntity> modificationsToMove = modifications.stream().map(originalModifications::remove).collect(Collectors.toList());

        List<ModificationEntity> newModificationList = new ArrayList<>(originalModifications.values());
        int index =  before == null ? newModificationList.size() : newModificationList.indexOf(originalModifications.get(before));
        newModificationList.addAll(index, modificationsToMove);

        modificationGroupEntity.setModifications(newModificationList);
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getId)
                .collect(Collectors.toList());
    }

    @Transactional
    public List<ModificationInfos> getModifications(List<UUID> uuids) {
        return this.modificationRepository.findAllById(uuids).stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata) {
        return onlyMetadata ? getModificationsMetadata(groupUuid) : getModificationsInfos(List.of(groupUuid));
    }

    private List<ModificationInfos> getModificationsMetadata(UUID groupUuid) {
        return modificationRepository
            .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
            .stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        return modificationRepository
            .findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString()))
            .toModificationInfos();
    }

    public Stream<ModificationEntity> getModificationList(UUID groupUuid) {
        return getModificationGroup(groupUuid).getModifications().stream().filter(Objects::nonNull);
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        if (!groupEntity.getModifications().isEmpty()) {
            modificationRepository.deleteAll(groupEntity.getModifications().stream().filter(Objects::nonNull).collect(Collectors.toList()));
        }
        this.modificationGroupRepository.delete(groupEntity);

    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public int deleteModifications(UUID groupUuid, Set<UUID> uuids) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        List<ModificationEntity> modifications = getModificationList(groupUuid)
                .filter(m -> uuids.contains(m.getId()))
                .collect(Collectors.toList());
        modifications.forEach(groupEntity::removeModification);
        int count = modifications.size();
        this.modificationRepository.deleteAll(modifications);
        return count;
    }

    public void updateModification(ModificationEntity modificationEntity) {
        this.modificationRepository.save(modificationEntity);
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()));
    }

    public EquipmentCreationEntity createLoadCreationEntity(String loadId, String loadName, LoadType loadType,
                                                            String voltageLevelId, String busOrBusbarSectionId, double activePower, double reactivePower) {
        return new LoadCreationEntity(loadId, loadName, loadType, voltageLevelId, busOrBusbarSectionId, activePower, reactivePower);
    }

    public EquipmentModificationEntity createLoadModificationEntity(String loadId, AttributeModification<String> loadName, AttributeModification<LoadType> loadType,
                                                                    AttributeModification<String> voltageLevelId, AttributeModification<String> busOrBusbarSectionId,
                                                                    AttributeModification<Double> activePower, AttributeModification<Double> reactivePower) {
        return new LoadModificationEntity(loadId, loadName, loadType, voltageLevelId, busOrBusbarSectionId, activePower, reactivePower);
    }

    public EquipmentCreationEntity createGeneratorEntity(String generatorId, String generatorName, EnergySource energySource,
                                                         String voltageLevelId, String busOrBusbarSectionId,
                                                         double minActivePower, double maxActivePower,
                                                         Double ratedNominalPower, double activePowerSetpoint,
                                                         Double reactivePowerSetpoint, boolean voltageRegulationOn, Double voltageSetpoint) {
        return new GeneratorCreationEntity(generatorId, generatorName, energySource, voltageLevelId, busOrBusbarSectionId, minActivePower,
            maxActivePower, ratedNominalPower, activePowerSetpoint, reactivePowerSetpoint, voltageRegulationOn, voltageSetpoint);
    }

    public EquipmentCreationEntity createLineEntity(String lineId, String lineName, double seriesResistance, double seriesReactance,
                                                    Double shuntConductance1, Double shuntSusceptance1, Double shuntConductance2, Double shuntSusceptance2,
                                                    String voltageLevelId1, String busOrBusbarSectionId1, String voltageLevelId2, String busOrBusbarSectionId2,
                                                    Double permanentCurrentLimit1, Double permanentCurrentLimit2) {
        return new LineCreationEntity(lineId, lineName, seriesResistance, seriesReactance,
                                        shuntConductance1, shuntSusceptance1, shuntConductance2, shuntSusceptance2,
                                        voltageLevelId1, busOrBusbarSectionId1, voltageLevelId2, busOrBusbarSectionId2,
                                        permanentCurrentLimit1, permanentCurrentLimit2);
    }

    public EquipmentCreationEntity createTwoWindingsTransformerEntity(String id, String name, double seriesResistance, double seriesReactance,
                                                    double magnetizingConductance, double magnetizingSusceptance, double ratedVoltage1, double ratedVoltage2,
                                                    String voltageLevelId1, String busOrBusbarSectionId1, String voltageLevelId2, String busOrBusbarSectionId2,
                                                    Double permanentCurrentLimit1, Double permanentCurrentLimit2) {
        return new TwoWindingsTransformerCreationEntity(id, name, seriesResistance, seriesReactance,
                magnetizingConductance, magnetizingSusceptance, ratedVoltage1, ratedVoltage2,
                voltageLevelId1, busOrBusbarSectionId1, voltageLevelId2, busOrBusbarSectionId2,
                permanentCurrentLimit1, permanentCurrentLimit2);
    }

    public EquipmentCreationEntity createSubstationEntity(String id, String name, Country country) {
        return new SubstationCreationEntity(id, name, country);
    }

    public VoltageLevelCreationEntity createVoltageLevelEntity(String id, String name, double nominalVoltage, String substationId,
        List<BusbarSectionCreationEmbeddable> busbarSections,
        List<BusbarConnectionCreationEmbeddable> busbarConnections) {
        return new VoltageLevelCreationEntity(id, name, nominalVoltage, substationId, busbarSections, busbarConnections);
    }

    public EquipmentDeletionEntity createEquipmentDeletionEntity(String equipmentId, String equipmentType) {
        return new EquipmentDeletionEntity(equipmentId, equipmentType);
    }

    public GroovyScriptModificationEntity createGroovyScriptModificationEntity(String script) {
        return new GroovyScriptModificationEntity(script);
    }

    public BranchStatusModificationEntity createBranchStatusModificationEntity(String lineId, BranchStatusModificationInfos.ActionType action) {
        return new BranchStatusModificationEntity(lineId, action);
    }

    @Transactional(readOnly = true)
    public List<ModificationEntity> getModificationsEntities(List<UUID> groupUuids) {
        return groupUuids.stream().flatMap(this::getModificationList).collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModificationsInfos(List<UUID> groupUuids) {
        return this.getModificationsEntities(groupUuids).stream().map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    public ShuntCompensatorCreationEntity createShuntCompensatorEntity(ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        return new ShuntCompensatorCreationEntity(shuntCompensatorCreationInfos);
    }

    public GeneratorModificationEntity createGeneratorModificationEntity(GeneratorModificationInfos generatorModificationInfos) {
        return new GeneratorModificationEntity(generatorModificationInfos);
    }
}
