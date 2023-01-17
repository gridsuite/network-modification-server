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
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
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

    private final Set<String> substationsIds = new HashSet<>();

    private final List<EquipmentDeletionInfos> deletions = new LinkedList<>();

    private final boolean isBuild;

    private final boolean isApplyModifications;

    public Set<String> getSubstationsIds() {
        return substationsIds;
    }

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

    public static Set<String> getSubstationIds(Identifiable<?> identifiable) {
        Set<String> ids = new HashSet<>();
        // TODO implement getVoltageLevels in powsybl
        if (identifiable instanceof Switch) {
            ids.add(((Switch) identifiable).getVoltageLevel().getSubstation().orElseThrow().getId());
        } else if (identifiable instanceof Injection) {
            ids.add(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getSubstation().orElseThrow().getId());
        } else if (identifiable instanceof Branch) {
            ids.add(((Branch<?>) identifiable).getTerminal1().getVoltageLevel().getSubstation().orElseThrow().getId());
            ids.add(((Branch<?>) identifiable).getTerminal2().getVoltageLevel().getSubstation().orElseThrow().getId());
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getSubstation().orElseThrow().getId());
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getSubstation().orElseThrow().getId());
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getSubstation().orElseThrow().getId());
        } else if (identifiable instanceof HvdcLine) {
            ids.add(((HvdcLine) identifiable).getConverterStation1().getTerminal().getVoltageLevel().getSubstation().orElseThrow().getId());
            ids.add(((HvdcLine) identifiable).getConverterStation2().getTerminal().getVoltageLevel().getSubstation().orElseThrow().getId());
        } else if (identifiable instanceof Substation) {
            ids.add(identifiable.getId());
        } else if (identifiable instanceof VoltageLevel) {
            ids.add(((VoltageLevel) identifiable).getSubstation().orElseThrow().getId());
        }
        return ids;
    }

    public Network getNetwork() {
        return network;
    }

    public boolean isBuild() {
        return isBuild;
    }

    public boolean isApplyModifications() {
        return isApplyModifications;
    }

    public List<ModificationInfos> getModifications() {
        List<ModificationInfos> modificationInfos = modifications.stream()
            .map(m -> {
                ModificationInfos infos = m.toModificationInfos();
                infos.setSubstationIds(substationsIds);
                return infos;
            }).collect(Collectors.toList());
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
                    .collect(Collectors.toList()));
        }
    }

    public List<EquipmentDeletionInfos> getDeletions() {
        return deletions;
    }

    public void storeBranchStatusModification(String lineId, BranchStatusModificationInfos.ActionType action) {
        modifications.add(this.modificationRepository.createBranchStatusModificationEntity(lineId, action));
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

    private void addSubstationsIds(Identifiable<?> identifiable) {
        substationsIds.addAll(getSubstationIds(identifiable));
    }
}
