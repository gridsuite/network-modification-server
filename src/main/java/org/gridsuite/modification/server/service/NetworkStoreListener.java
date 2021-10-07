/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.EquipmentType;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
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

    private final List<EquipmentModificationEntity> modifications = new LinkedList<>();

    private Set<String> substationsIds = new HashSet<>();

    Network getNetwork() {
        return network;
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, UUID groupUuid,
                                              NetworkModificationRepository modificationRepository, EquipmentInfosService equipmentInfosService) {
        var listener = new NetworkStoreListener(network, networkUuid, groupUuid, modificationRepository, equipmentInfosService);
        network.addListener(listener);
        return listener;
    }

    protected NetworkStoreListener(Network network, UUID networkUuid, UUID groupUuid,
                                   NetworkModificationRepository modificationRepository, EquipmentInfosService equipmentInfosService) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.groupUuid = groupUuid;
        this.modificationRepository = modificationRepository;
        this.equipmentInfosService = equipmentInfosService;
    }

    public List<EquipmenModificationInfos> getModifications() {
        return modifications.stream()
            .map(m -> m.toEquipmentModificationInfos(substationsIds.isEmpty() ? getSubstationsIds(m.getEquipmentId())
                : substationsIds))
                .collect(Collectors.toList());
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

    private void storeEquipmentAttributeModification(Identifiable<?> identifiable, String attributeName, Object attributeValue) {
        modifications.add(this.modificationRepository.createEquipmentAttributeModification(identifiable.getId(), attributeName, attributeValue));
    }

    public void storeLoadCreation(LoadCreationInfos loadCreationInfos) {
        modifications.add(this.modificationRepository.createLoadEntity(loadCreationInfos.getEquipmentId(),
            loadCreationInfos.getEquipmentName(),
            loadCreationInfos.getLoadType(),
            loadCreationInfos.getVoltageLevelId(),
            loadCreationInfos.getBusId(),
            loadCreationInfos.getActivePower(),
            loadCreationInfos.getReactivePower()));
    }

    public void storeEquipmentDeletion(String equipmentId, String equipmentType) {
        modifications.add(this.modificationRepository.createEquipmentDeletionEntity(equipmentId, equipmentType));
    }

    private Set<String> getSubstationsIds(String equipmentId) {
        Identifiable<?> identifiable = network.getIdentifiable(equipmentId);
        Set<String> substationsIds = new HashSet<>();
        if (identifiable instanceof Switch) {
            substationsIds.add(((Switch) identifiable).getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Injection) {
            substationsIds.add(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Branch) {
            substationsIds.add(((Branch<?>) identifiable).getTerminal1().getVoltageLevel().getSubstation().getId());
            substationsIds.add(((Branch<?>) identifiable).getTerminal2().getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getSubstation().getId());
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getSubstation().getId());
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getSubstation().getId());
        }

        return substationsIds;
    }

    public static Set<String> getSubstationIds(Identifiable identifiable) {
        Set<String> substationsIds = new HashSet<>();
        if (identifiable instanceof Switch) {
            substationsIds.add(((Switch) identifiable).getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Injection) {
            substationsIds.add(((Injection<?>) identifiable).getTerminal().getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Branch) {
            substationsIds.add(((Branch<?>) identifiable).getTerminal1().getVoltageLevel().getSubstation().getId());
            substationsIds.add(((Branch<?>) identifiable).getTerminal2().getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.ONE).getVoltageLevel().getSubstation().getId());
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.TWO).getVoltageLevel().getSubstation().getId());
            substationsIds.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeWindingsTransformer.Side.THREE).getVoltageLevel().getSubstation().getId());
        }
        return substationsIds;
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        storeEquipmentAttributeModification(identifiable, attribute, newValue);
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        storeEquipmentAttributeModification(identifiable, attribute, newValue);
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        equipmentInfosService.add(
            EquipmentInfos.builder()
                .networkUuid(networkUuid)
                .equipmentId(identifiable.getId())
                .equipmentName(identifiable.getNameOrId())
                .equipmentType(EquipmentType.getType(identifiable).name())
                .build()
        );
    }

    @Override
    public void onRemoval(Identifiable identifiable) {
        // We cannot delete equipments infos here yet :
        // identifiable.getId() throws PowsyblException("Object has been removed in current variant");
        // because the identifiable resource was set to null in remove method, before calling onRemoval method
        //
        // onRemoval must be changed in powsybl core (maybe passing only a String as argument)
        //equipmentInfosService.delete(identifiable.getId(), networkUuid);
    }

    public void setSubstationsIds(Set<String> substationsIds) {
        this.substationsIds = substationsIds;
    }

    public void deleteEquipmentInfos(String equipmentId) {
        equipmentInfosService.delete(equipmentId, networkUuid);
    }
}
