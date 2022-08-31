/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.*;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineSplitWithVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import static org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity.toEmbeddablePoints;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkStoreListener implements NetworkListener {

    private final UUID groupUuid;

    private final UUID networkUuid;

    private final Network network;

    private final NetworkModificationRepository modificationRepository;

    private final EquipmentInfosService equipmentInfosService;

    private final List<ModificationEntity> modifications = new LinkedList<>();

    private final Set<String> substationsIds = new HashSet<>();

    private final List<EquipmentDeletionInfos> deletions = new ArrayList<>();

    private final boolean isBuild;

    private final boolean isApplyModifications;

    protected NetworkStoreListener(Network network, UUID networkUuid, UUID groupUuid,
                                   NetworkModificationRepository modificationRepository, EquipmentInfosService equipmentInfosService,
                                   boolean isBuild, boolean isApplyModifications) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.groupUuid = groupUuid;
        this.modificationRepository = modificationRepository;
        this.equipmentInfosService = equipmentInfosService;
        this.isBuild = isBuild;
        this.isApplyModifications = isApplyModifications;
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, UUID groupUuid,
                                              NetworkModificationRepository modificationRepository,
                                              EquipmentInfosService equipmentInfosService,
                                              boolean isBuild, boolean isApplyModifications) {
        var listener = new NetworkStoreListener(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService,
            isBuild, isApplyModifications);
        network.addListener(listener);
        return listener;
    }

    public static Set<String> getSubstationIds(Identifiable identifiable) {
        Set<String> ids = new HashSet<>();
        if (identifiable instanceof Switch) {
            ids.add(((Switch) identifiable).getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
        } else if (identifiable instanceof Injection) {
            ids.add(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
        } else if (identifiable instanceof Branch) {
            ids.add(((Branch<?>) identifiable).getTerminal1().getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
            ids.add(((Branch<?>) identifiable).getTerminal2().getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
        } else if (identifiable instanceof HvdcLine) {
            ids.add(((HvdcLine) identifiable).getConverterStation1().getTerminal().getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
            ids.add(((HvdcLine) identifiable).getConverterStation2().getTerminal().getVoltageLevel().getSubstation().orElseThrow().getId()); // TODO
        } else if (identifiable instanceof Substation) {
            ids.add(identifiable.getId());
        } else if (identifiable instanceof VoltageLevel) {
            ids.add(((VoltageLevel) identifiable).getSubstation().orElseThrow().getId());
        }
        return ids;
    }

    Network getNetwork() {
        return network;
    }

    UUID getNetworkUuid() {
        return networkUuid;
    }

    boolean isBuild() {
        return isBuild;
    }

    boolean isApplyModifications() {
        return isApplyModifications;
    }

    public List<ModificationInfos> getModifications() {
        List<ModificationInfos> modificationInfos = modifications.stream()
            .map(m -> {
                ModificationInfos infos = m.toModificationInfos();
                infos.setSubstationIds(substationsIds);
                return infos;
            })
                .collect(Collectors.toList());
        modifications.clear();
        return modificationInfos;
    }

    public void saveModifications() {
        if (groupUuid != null) {
            modificationRepository.saveModifications(groupUuid,
                modifications
                    .stream()
                    .map(ModificationEntity.class::cast)
                    .collect(Collectors.toList()));
        }
    }

    public void deleteModifications() {
        if (groupUuid != null) {
            modificationRepository.deleteModifications(groupUuid,
                modifications
                    .stream()
                    .map(ModificationEntity::getId)
                    .collect(Collectors.toSet()));
        }
    }

    public Collection<EquipmentDeletionInfos> getDeletions() {
        return Collections.unmodifiableCollection(deletions);
    }

    public void storeEquipmentAttributeModification(Identifiable<?> identifiable, String attributeName, Object attributeValue) {
        modifications.add(this.modificationRepository.createEquipmentAttributeModification(identifiable.getId(), attributeName, attributeValue));
    }

    public void storeEquipmentAttributeModification(String equipmentId, String attributeName, Object attributeValue) {
        modifications.add(this.modificationRepository.createEquipmentAttributeModification(equipmentId, attributeName, attributeValue));
    }

    public void storeLoadCreation(LoadCreationInfos loadCreationInfos) {
        modifications.add(this.modificationRepository.createLoadCreationEntity(loadCreationInfos.getEquipmentId(),
                loadCreationInfos.getEquipmentName(),
                loadCreationInfos.getLoadType(),
                loadCreationInfos.getVoltageLevelId(),
                loadCreationInfos.getBusOrBusbarSectionId(),
                loadCreationInfos.getActivePower(),
                loadCreationInfos.getReactivePower()));
    }

    public void storeLoadModification(LoadModificationInfos loadModificationInfos) {
        modifications.add(this.modificationRepository.createLoadModificationEntity(loadModificationInfos.getEquipmentId(),
                loadModificationInfos.getEquipmentName(),
                loadModificationInfos.getLoadType(),
                loadModificationInfos.getVoltageLevelId(),
                loadModificationInfos.getBusOrBusbarSectionId(),
                loadModificationInfos.getActivePower(),
                loadModificationInfos.getReactivePower()));
    }

    @Transactional
    public void storeGeneratorModification(GeneratorModificationInfos generatorModificationInfos) {
        modifications.add(new GeneratorModificationEntity(generatorModificationInfos));
    }

    public void storeGeneratorCreation(GeneratorCreationInfos generatorCreationInfos) {
        modifications.add(this.modificationRepository.createGeneratorEntity(generatorCreationInfos.getEquipmentId(),
            generatorCreationInfos.getEquipmentName(),
            generatorCreationInfos.getEnergySource(),
            generatorCreationInfos.getVoltageLevelId(),
            generatorCreationInfos.getBusOrBusbarSectionId(),
            generatorCreationInfos.getMinActivePower(),
            generatorCreationInfos.getMaxActivePower(),
            generatorCreationInfos.getRatedNominalPower(),
            generatorCreationInfos.getActivePowerSetpoint(),
            generatorCreationInfos.getReactivePowerSetpoint(),
            generatorCreationInfos.isVoltageRegulationOn(),
            generatorCreationInfos.getVoltageSetpoint(),
            generatorCreationInfos.getMarginalCost(),
            generatorCreationInfos.getMinimumReactivePower(),
            generatorCreationInfos.getMaximumReactivePower(),
            generatorCreationInfos.getParticipate(),
            generatorCreationInfos.getDroop(),
            generatorCreationInfos.getTransientReactance(),
            generatorCreationInfos.getStepUpTransformerReactance(),
            toEmbeddablePoints(generatorCreationInfos.getPoints())));
    }

    public void storeEquipmentDeletion(String equipmentId, String equipmentType) {
        modifications.add(this.modificationRepository.createEquipmentDeletionEntity(equipmentId, equipmentType));
    }

    public void storeLineCreation(LineCreationInfos lineCreationInfos) {
        modifications.add(this.modificationRepository.createLineEntity(lineCreationInfos.getEquipmentId(),
            lineCreationInfos.getEquipmentName(),
            lineCreationInfos.getSeriesResistance(),
            lineCreationInfos.getSeriesReactance(),
            lineCreationInfos.getShuntConductance1(),
            lineCreationInfos.getShuntSusceptance1(),
            lineCreationInfos.getShuntConductance2(),
            lineCreationInfos.getShuntSusceptance2(),
            lineCreationInfos.getVoltageLevelId1(),
            lineCreationInfos.getBusOrBusbarSectionId1(),
            lineCreationInfos.getVoltageLevelId2(),
            lineCreationInfos.getBusOrBusbarSectionId2(),
            lineCreationInfos.getCurrentLimits1() != null ? lineCreationInfos.getCurrentLimits1().getPermanentLimit() : null,
            lineCreationInfos.getCurrentLimits2() != null ? lineCreationInfos.getCurrentLimits2().getPermanentLimit() : null
        ));
    }

    public void storeTwoWindingsTransformerCreation(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        modifications.add(this.modificationRepository.createTwoWindingsTransformerEntity(twoWindingsTransformerCreationInfos.getEquipmentId(),
                twoWindingsTransformerCreationInfos.getEquipmentName(),
                twoWindingsTransformerCreationInfos.getSeriesResistance(),
                twoWindingsTransformerCreationInfos.getSeriesReactance(),
                twoWindingsTransformerCreationInfos.getMagnetizingConductance(),
                twoWindingsTransformerCreationInfos.getMagnetizingSusceptance(),
                twoWindingsTransformerCreationInfos.getRatedVoltage1(),
                twoWindingsTransformerCreationInfos.getRatedVoltage2(),
                twoWindingsTransformerCreationInfos.getVoltageLevelId1(),
                twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId1(),
                twoWindingsTransformerCreationInfos.getVoltageLevelId2(),
                twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId2(),
                twoWindingsTransformerCreationInfos.getCurrentLimits1() != null ? twoWindingsTransformerCreationInfos.getCurrentLimits1().getPermanentLimit() : null,
                twoWindingsTransformerCreationInfos.getCurrentLimits2() != null ? twoWindingsTransformerCreationInfos.getCurrentLimits2().getPermanentLimit() : null)
        );
    }

    public void storeShuntCompensatorCreation(ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        modifications.add(this.modificationRepository.createShuntCompensatorEntity(shuntCompensatorCreationInfos));
    }

    public void storeGroovyScriptModification(String script) {
        modifications.add(this.modificationRepository.createGroovyScriptModificationEntity(script));
    }

    public void storeBranchStatusModification(String lineId, BranchStatusModificationInfos.ActionType action) {
        modifications.add(this.modificationRepository.createBranchStatusModificationEntity(lineId, action));
    }

    public void storeSubstationCreation(SubstationCreationInfos substationCreationInfos) {
        modifications.add(this.modificationRepository.createSubstationEntity(
                substationCreationInfos.getEquipmentId(),
                substationCreationInfos.getEquipmentName(),
                substationCreationInfos.getSubstationCountry()
        ));
    }

    public void storeVoltageLevelCreation(VoltageLevelCreationInfos voltageLevelCreationInfos) {
        VoltageLevelCreationEntity voltageLevelEntity = VoltageLevelCreationEntity.toEntity(voltageLevelCreationInfos);
        modifications.add(voltageLevelEntity);
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        substationsIds.addAll(getSubstationIds(identifiable));
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        substationsIds.addAll(getSubstationIds(identifiable));
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        substationsIds.addAll(getSubstationIds(identifiable));
        equipmentInfosService.addEquipmentInfos(
            EquipmentInfos.builder()
                .networkUuid(networkUuid)
                .variantId(network.getVariantManager().getWorkingVariantId())
                .id(identifiable.getId())
                .name(identifiable.getNameOrId())
                .type(identifiable.getType().name())
                .voltageLevels(EquipmentInfos.getVoltageLevels(identifiable))
                .build()
        );
    }

    @Override
    public void beforeRemoval(Identifiable identifiable) {
        EquipmentDeletionInfos di = EquipmentDeletionInfos
            .builder()
            .uuid(null) // not in "this" db, transient
            .date(ZonedDateTime.now(ZoneOffset.UTC))
            .type(ModificationType.EQUIPMENT_DELETION)
            .equipmentId(identifiable.getId())
            .equipmentType(identifiable.getType().name())
            .build();
        addSubstationsIds(identifiable);
        this.deletions.add(di);
    }

    @Override
    public void afterRemoval(String id) {
        String variantId = network.getVariantManager().getWorkingVariantId();
        if (equipmentInfosService.existEquipmentInfos(id, networkUuid, variantId)) {
            equipmentInfosService.deleteEquipmentInfos(id, networkUuid, variantId);
        } else {
            equipmentInfosService.addTombstonedEquipmentInfos(
                TombstonedEquipmentInfos.builder()
                    .networkUuid(networkUuid)
                    .variantId(variantId)
                    .id(id)
                    .build()
            );
        }
    }

    public void addSubstationsIds(Identifiable identifiable) {
        substationsIds.addAll(getSubstationIds(identifiable));
    }

    public void storeLineSplitWithVoltageLevelInfos(LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        VoltageLevelCreationInfos mayNewVoltageLevelInfos = lineSplitWithVoltageLevelInfos.getMayNewVoltageLevelInfos();

        modifications.add(LineSplitWithVoltageLevelEntity.toEntity(
            lineSplitWithVoltageLevelInfos.getLineToSplitId(),
            lineSplitWithVoltageLevelInfos.getPercent(),
            mayNewVoltageLevelInfos,
            lineSplitWithVoltageLevelInfos.getExistingVoltageLevelId(),
            lineSplitWithVoltageLevelInfos.getBbsOrBusId(),
            lineSplitWithVoltageLevelInfos.getNewLine1Id(),
            lineSplitWithVoltageLevelInfos.getNewLine1Name(),
            lineSplitWithVoltageLevelInfos.getNewLine2Id(),
            lineSplitWithVoltageLevelInfos.getNewLine2Name())
        );
    }

    public void storeLineAttachToVoltageLevelInfos(LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        VoltageLevelCreationInfos mayNewVoltageLevelInfos = lineAttachToVoltageLevelInfos.getMayNewVoltageLevelInfos();
        LineCreationInfos attachmentLine = lineAttachToVoltageLevelInfos.getAttachmentLine();

        modifications.add(LineAttachToVoltageLevelEntity.toEntity(
                lineAttachToVoltageLevelInfos.getLineToAttachToId(),
                lineAttachToVoltageLevelInfos.getPercent(),
                lineAttachToVoltageLevelInfos.getAttachmentPointId(),
                lineAttachToVoltageLevelInfos.getAttachmentPointName(),
                mayNewVoltageLevelInfos,
                lineAttachToVoltageLevelInfos.getExistingVoltageLevelId(),
                lineAttachToVoltageLevelInfos.getBbsOrBusId(),
                attachmentLine,
                lineAttachToVoltageLevelInfos.getNewLine1Id(),
                lineAttachToVoltageLevelInfos.getNewLine1Name(),
                lineAttachToVoltageLevelInfos.getNewLine2Id(),
                lineAttachToVoltageLevelInfos.getNewLine2Name())
        );
    }
}
