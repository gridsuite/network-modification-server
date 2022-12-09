/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.NonNull;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.GroovyScriptModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.modification.BranchStatusModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MOVE_MODIFICATION_ERROR;

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

    @Transactional // To have all create in the same transaction (atomic)
    public void saveModifications(UUID groupUuid, List<? extends ModificationEntity> modifications) {
        var modificationGroupEntity = this.modificationGroupRepository
                .findById(groupUuid)
                .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        modifications.forEach(modificationGroupEntity::addModification);
    }

    @Transactional // To have all move in the same transaction (atomic)
    //when we move modifications, we move them right before referenceModification when it is defined, at the end of list otherwise
    public List<UUID> moveModifications(UUID destinationGroupUuid, UUID originGroupUuid, List<UUID> modificationsUuid, UUID referenceModificationUuid) {
        ModificationGroupEntity originModificationGroupEntity = getModificationGroup(originGroupUuid);

        Map<UUID, ModificationEntity> originModifications = modificationRepository.findAllBaseByGroupId(originGroupUuid).stream()
                .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

        List<UUID> modificationsInErrorUUID = modificationsUuid.stream().filter(mUuid -> !originModifications.containsKey(mUuid)).collect(Collectors.toList());
        List<UUID> modificationsToMoveUUID = modificationsUuid.stream().filter(originModifications::containsKey).collect(Collectors.toList());

        // if moving within the same group
        if (originGroupUuid.equals(destinationGroupUuid)) {
            if (referenceModificationUuid != null && !originModifications.containsKey(referenceModificationUuid)) {
                throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
            }

            List<ModificationEntity> newDestinationModificationList = updateModificationList(modificationsToMoveUUID, originModifications, originModifications, referenceModificationUuid);

            originModificationGroupEntity.setModifications(newDestinationModificationList);
        } else {
            //if destination is empty, group does not exist, we create it here if needed
            ModificationGroupEntity destinationModificationGroupEntity = getOrCreateModificationGroup(destinationGroupUuid);

            Map<UUID, ModificationEntity> destinationModifications = modificationRepository.findAllBaseByGroupId(destinationGroupUuid).stream()
                                                                         .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

            // referenceModificationUuid must belong to destination one
            if (referenceModificationUuid != null && !destinationModifications.containsKey(referenceModificationUuid)) {
                throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
            }

            List<ModificationEntity> newDestinationModificationList = updateModificationList(modificationsToMoveUUID, originModifications, destinationModifications, referenceModificationUuid);

            originModificationGroupEntity.setModifications(new ArrayList<>(originModifications.values()));
            destinationModificationGroupEntity.setModifications(newDestinationModificationList);
        }

        return modificationsInErrorUUID;
    }

    public List<ModificationEntity> updateModificationList(List<UUID> modificationsToMoveUuid, Map<UUID, ModificationEntity> originModifications, Map<UUID, ModificationEntity> destinationModifications, UUID referenceModificationUuid) {
        List<ModificationEntity> movedModifications = modificationsToMoveUuid.stream().map(originModifications::remove).collect(Collectors.toList());

        List<ModificationEntity> newDestinationModificationList = new ArrayList<>(destinationModifications.values());
        /* when referenceModification == null we move at the end of list */
        int index =  referenceModificationUuid == null ? newDestinationModificationList.size() : newDestinationModificationList.indexOf(destinationModifications.get(referenceModificationUuid));
        newDestinationModificationList.addAll(index, movedModifications);

        return newDestinationModificationList;
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getId)
                .collect(Collectors.toList());
    }

    public ModificationInfos getModification(UUID uuid) {
        return this.modificationRepository.findById(uuid).get().toModificationInfos();
    }

    public List<ModificationInfos> getModifications(List<UUID> uuids) {
        return this.modificationRepository.findAllById(uuids).stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        try {
            return onlyMetadata ? getModificationsMetadata(groupUuid) : getModificationsInfos(List.of(groupUuid));
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return List.of();
            }
            throw e;
        }
    }

    private List<ModificationInfos> getModificationsMetadata(UUID groupUuid) {
        return modificationRepository
            .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
            .stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    public List<ModificationInfos> getModificationsInfos(List<UUID> groupUuids) {
        return this.getModificationsEntities(groupUuids).stream().map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        return modificationRepository
            .findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString()))
            .toModificationInfos();
    }

    public Stream<ModificationEntity> getModificationEntityList(UUID groupUuid) {
        return getModificationGroup(groupUuid).getModifications().stream().filter(Objects::nonNull);
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            if (!groupEntity.getModifications().isEmpty()) {
                modificationRepository.deleteAll(groupEntity.getModifications().stream().filter(Objects::nonNull).collect(Collectors.toList()));
            }
            this.modificationGroupRepository.delete(groupEntity);
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return;
            }
            throw e;
        }
    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public int deleteModifications(UUID groupUuid, List<UUID> uuids) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        List<ModificationEntity> modifications = getModificationEntityList(groupUuid)
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

    private ModificationGroupEntity getOrCreateModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
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
                                                         Double reactivePowerSetpoint, boolean voltageRegulationOn, Double voltageSetpoint,
                                                         Double marginalCost, Double minQ, Double maxQ, boolean participate, Float droop,
                                                         Double transientReactance, Double stepUpTransformerReactance,
                                                         String regulatingTerminalId, String regulatingTerminalType, String regulatingTerminalVlId,
                                                         Double qPercent, boolean reactiveCapabilityCurve,
                                                         List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints, String connectionName,
                                                         ConnectablePosition.Direction connectionDirection) {
        return new GeneratorCreationEntity(generatorId, generatorName, energySource, voltageLevelId, busOrBusbarSectionId, minActivePower,
            maxActivePower, ratedNominalPower, activePowerSetpoint, reactivePowerSetpoint, voltageRegulationOn, voltageSetpoint, marginalCost, minQ, maxQ,
            participate, droop,  transientReactance, stepUpTransformerReactance, reactiveCapabilityCurvePoints, regulatingTerminalId, regulatingTerminalType,
            regulatingTerminalVlId, qPercent, reactiveCapabilityCurve, connectionName, connectionDirection);
    }

    public EquipmentCreationEntity createLineEntity(String lineId, String lineName, double seriesResistance, double seriesReactance,
                                                    Double shuntConductance1, Double shuntSusceptance1, Double shuntConductance2, Double shuntSusceptance2,
                                                    String voltageLevelId1, String busOrBusbarSectionId1, String voltageLevelId2, String busOrBusbarSectionId2,
                                                    Double permanentCurrentLimit1, Double permanentCurrentLimit2, String connectionName1, ConnectablePosition.Direction connectionDirection1, String connectionName2,
                                                    ConnectablePosition.Direction connectionDirection2) {
        return new LineCreationEntity(lineId, lineName, seriesResistance, seriesReactance,
                                        shuntConductance1, shuntSusceptance1, shuntConductance2, shuntSusceptance2,
                                        voltageLevelId1, busOrBusbarSectionId1, voltageLevelId2, busOrBusbarSectionId2,
                                        permanentCurrentLimit1, permanentCurrentLimit2, connectionName1, connectionDirection1,
                                        connectionName2, connectionDirection2);
    }

    public EquipmentCreationEntity createTwoWindingsTransformerEntity(String id, String name, double seriesResistance, double seriesReactance,
                                                                      double magnetizingConductance, double magnetizingSusceptance, double ratedVoltage1, double ratedVoltage2, double ratedS,
                                                                      String voltageLevelId1, String busOrBusbarSectionId1, String voltageLevelId2, String busOrBusbarSectionId2,
                                                                      Double permanentCurrentLimit1, Double permanentCurrentLimit2,
                                                                      String connectionName1,
                                                                      ConnectablePosition.Direction connectionDirection1,
                                                                      String connectionName2,
                                                                      ConnectablePosition.Direction connectionDirection2,
                                                                      Integer phaseTapChangerLowTapPosition,
                                                                      Integer phaseTapChangerTapPosition,
                                                                      Boolean phaseTapChangerRegulating,
                                                                      Double phaseTapChangerTargetDeadband,
                                                                      String phaseTapChangerTerminalRefConnectableId,
                                                                      String phaseTapChangerTerminalRefVoltageLevelId,
                                                                      String phaseTapChangerTerminalRefType,
                                                                      PhaseTapChanger.RegulationMode phaseTapChangerRegulationMode,
                                                                      Double phaseTapChangerRegulationValue,
                                                                      Integer ratioTapChangerLowTapPosition,
                                                                      Integer ratioTapChangerTapPosition,
                                                                      Boolean ratioTapChangerRegulating,
                                                                      Double ratioTapChangerTargetDeadband,
                                                                      String ratioTapChangerTerminalRefConnectableId,
                                                                      String ratioTapChangerTerminalRefVoltageLevelId,
                                                                      String ratioTapChangerTerminalRefType,
                                                                      Boolean ratioTapChangerLoadTapChangingCapabilities,
                                                                      Double ratioTapChangerTargetV,
                                                                      List<TapChangerStepCreationEmbeddable> tapChangerSteps) {
        return new TwoWindingsTransformerCreationEntity(id, name, seriesResistance, seriesReactance,
                magnetizingConductance, magnetizingSusceptance, ratedVoltage1, ratedVoltage2, ratedS,
                voltageLevelId1, busOrBusbarSectionId1, voltageLevelId2, busOrBusbarSectionId2,
                permanentCurrentLimit1, permanentCurrentLimit2,
                connectionName1, connectionDirection1,
                connectionName2, connectionDirection2,
                phaseTapChangerLowTapPosition,
                phaseTapChangerTapPosition,
                phaseTapChangerRegulating,
                phaseTapChangerTargetDeadband,
                phaseTapChangerTerminalRefConnectableId,
                phaseTapChangerTerminalRefVoltageLevelId,
                phaseTapChangerTerminalRefType,
                phaseTapChangerRegulationMode,
                phaseTapChangerRegulationValue,
                ratioTapChangerLowTapPosition,
                ratioTapChangerTapPosition,
                ratioTapChangerRegulating,
                ratioTapChangerTargetDeadband,
                ratioTapChangerTerminalRefConnectableId,
                ratioTapChangerTerminalRefVoltageLevelId,
                ratioTapChangerTerminalRefType,
                ratioTapChangerLoadTapChangingCapabilities,
                ratioTapChangerTargetV,
                tapChangerSteps);
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

    private List<ModificationEntity> getModificationsEntities(List<UUID> groupUuids) {
        return groupUuids.stream().flatMap(this::getModificationEntityList).collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public Optional<ModificationEntity> cloneModificationEntity(UUID modificationUuid) {
        Optional<ModificationEntity> entity = modificationRepository.findById(modificationUuid);
        entity.ifPresent(ModificationEntity::cloneWithIdsToNull);
        return entity;
    }

    @Transactional(readOnly = true)
    public List<ModificationEntity> cloneModificationsEntities(@NonNull UUID groupUuid) {
        return getModificationEntityList(groupUuid).map(entity -> {
            entity.cloneWithIdsToNull();
            return entity;
        }).collect(Collectors.toList());
    }

    public GeneratorModificationEntity createGeneratorModificationEntity(GeneratorModificationInfos generatorModificationInfos) {
        return new GeneratorModificationEntity(generatorModificationInfos);
    }
}
