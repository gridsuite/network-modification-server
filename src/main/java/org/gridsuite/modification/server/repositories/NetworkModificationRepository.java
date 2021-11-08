/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmenAttributeModificationInfos;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.attribute.modification.BooleanEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.attribute.modification.DoubleEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.attribute.modification.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.attribute.modification.FloatEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.attribute.modification.IntegerEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.attribute.modification.StringEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.EquipmentCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;

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
    public void saveModifications(UUID groupUuid, List<ModificationEntity> modifications) {
        var modificationGroupEntity = this.modificationGroupRepository
                .findById(groupUuid)
                .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        modifications.forEach(m -> m.setGroup(modificationGroupEntity));
        this.modificationRepository.saveAll(modifications);
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getId)
                .collect(Collectors.toList());
    }

    public List<ModificationInfos> getModifications(UUID groupUuid) {
        ModificationGroupEntity group = getModificationGroup(groupUuid);
        return this.modificationRepository.findAllBaseByGroupId(group.getId())
                .stream()
                .map(ModificationEntity::toModificationInfos)
                .collect(Collectors.toList());
    }

    public EquipmenAttributeModificationInfos getEquipmentAttributeModification(UUID groupUuid, UUID modificationUuid) {
        return ((EquipmentAttributeModificationEntity<?>) this.modificationRepository
                .findById(modificationUuid)
                .filter(m -> ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION.name().equals(m.getType()))
                .filter(m -> groupUuid.equals(m.getGroup().getId()))
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString())))
                .toEquipmentAttributeModificationInfos();
    }

    public LoadCreationInfos getLoadCreationModification(UUID groupUuid, UUID modificationUuid) {
        return ((LoadCreationEntity) this.modificationRepository
            .findById(modificationUuid)
            .filter(m -> ModificationType.LOAD_CREATION.name().equals(m.getType()))
            .filter(m -> groupUuid.equals(m.getGroup().getId()))
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString())))
            .toLoadCreationInfos();
    }

    public GeneratorCreationInfos getGeneratorCreationModification(UUID groupUuid, UUID modificationUuid) {
        return ((GeneratorCreationEntity) this.modificationRepository
            .findById(modificationUuid)
            .filter(m -> ModificationType.GENERATOR_CREATION.name().equals(m.getType()))
            .filter(m -> groupUuid.equals(m.getGroup().getId()))
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString())))
            .toGeneratorCreationInfos();
    }

    @Transactional
    public LineCreationInfos getLineCreationModification(UUID groupUuid, UUID modificationUuid) {
        return ((LineCreationEntity) this.modificationRepository
            .findById(modificationUuid)
            .filter(m -> ModificationType.LINE_CREATION.name().equals(m.getType()))
            .filter(m -> groupUuid.equals(m.getGroup().getId()))
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString())))
            .toLineCreationInfos();
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        this.modificationRepository.deleteAll(this.modificationRepository.findAllByGroupId(groupUuid));
        this.modificationGroupRepository.delete(groupEntity);
    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public void deleteModifications(UUID groupUuid, Set<UUID> uuids) {
        List<ModificationEntity> modifications = this.modificationRepository.findAllByGroupId(groupUuid)
                .stream()
                .filter(m -> uuids.contains(m.getId()))
                .collect(Collectors.toList());
        this.modificationRepository.deleteAll(modifications);
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()));
    }

    public EquipmentCreationEntity createLoadEntity(String loadId, String loadName, LoadType loadType,
                                                    String voltageLevelId, String busOrBusbarSectionId, double activePower, double reactivePower) {
        return new LoadCreationEntity(loadId, loadName, loadType, voltageLevelId, busOrBusbarSectionId, activePower, reactivePower);
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

    public EquipmentDeletionEntity createEquipmentDeletionEntity(String equipmentId, String equipmentType) {
        return new EquipmentDeletionEntity(equipmentId, equipmentType);
    }
}
