/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.BusbarConnectionCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.creation.BusbarSectionCreationEmbeddable;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;

import java.util.*;
import java.util.stream.Collectors;

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

    private Set<String> substationsIds = new HashSet<>();

    private boolean isBuild;

    private boolean isApplyModifications;

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

    public void storeEquipmentAttributeModification(Identifiable<?> identifiable, String attributeName, Object attributeValue) {
        modifications.add(this.modificationRepository.createEquipmentAttributeModification(identifiable.getId(), attributeName, attributeValue));
    }

    public void storeEquipmentAttributeModification(String equipmentId, String attributeName, Object attributeValue) {
        modifications.add(this.modificationRepository.createEquipmentAttributeModification(equipmentId, attributeName, attributeValue));
    }

    public void storeLoadCreation(LoadCreationInfos loadCreationInfos) {
        modifications.add(this.modificationRepository.createLoadEntity(loadCreationInfos.getEquipmentId(),
            loadCreationInfos.getEquipmentName(),
            loadCreationInfos.getLoadType(),
            loadCreationInfos.getVoltageLevelId(),
            loadCreationInfos.getBusOrBusbarSectionId(),
            loadCreationInfos.getActivePower(),
            loadCreationInfos.getReactivePower()));
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
            generatorCreationInfos.getVoltageSetpoint()));
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
        List<BusbarSectionCreationEmbeddable> bbsEmbeddables = voltageLevelCreationInfos.getBusbarSections().stream().map(bbsi -> {
            return new BusbarSectionCreationEmbeddable(bbsi.getId(), bbsi.getName(), bbsi.getVertPos(), bbsi.getHorizPos());
        }).collect(Collectors.toList());
        List<BusbarConnectionCreationEmbeddable> cnxEmbeddables = voltageLevelCreationInfos.getBusbarConnections().stream().map(cnxi -> {
            return new BusbarConnectionCreationEmbeddable(cnxi.getFromBBS(), cnxi.getToBBS(), cnxi.getSwitchKind());
        }).collect(Collectors.toList());
        modifications.add(this.modificationRepository.createVoltageLevelEntity(
            voltageLevelCreationInfos.getEquipmentId(),
            voltageLevelCreationInfos.getEquipmentName(),
            voltageLevelCreationInfos.getNominalVoltage(),
            voltageLevelCreationInfos.getSubstationId(),
            bbsEmbeddables,
            cnxEmbeddables
        ));
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
    public void onVariantCreated(String sourceVariantId, String targetVariantId) {
        // Initial variant modifications are not cloned
        if (sourceVariantId != VariantManagerConstants.INITIAL_VARIANT_ID) {
            equipmentInfosService.cloneVariantModifications(networkUuid, sourceVariantId, targetVariantId);
        }
    }

    @Override
    public void onVariantRemoved(String variantId) {
        equipmentInfosService.deleteVariants(networkUuid, List.of(variantId));
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        substationsIds.addAll(getSubstationIds(identifiable));
        equipmentInfosService.add(
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
        // At the moment, we cannot delete equipments infos in elasticsearch here :
        // identifiable.getId() throws PowsyblException("Object has been removed in current variant");
        // because the identifiable resource was set to null in remove method, before calling onRemoval method
        // onRemoval must be changed in powsybl core (maybe passing only the id as string argument)
        //equipmentInfosService.delete(identifiable.getId(), networkUuid);
    }

    @Override
    public void afterRemoval(String id) {
        // nothing to do
    }

    public void onTemporaryRemoval(String equipmentId, Set<String> substationsIds) {
        this.substationsIds.addAll(substationsIds);
        String variantId = network.getVariantManager().getWorkingVariantId();
        if (equipmentInfosService.existEquipmentInVariant(equipmentId, networkUuid, variantId)) {
            equipmentInfosService.delete(equipmentId, networkUuid, variantId);
        } else {
            equipmentInfosService.add(
                    EquipmentInfos.builder()
                            .networkUuid(networkUuid)
                            .variantId(variantId)
                            .id(equipmentId)
                            .tombstoned(true)
                            .build()
            );
        }
    }

}
