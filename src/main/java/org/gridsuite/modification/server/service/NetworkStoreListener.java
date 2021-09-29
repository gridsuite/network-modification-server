/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import java.util.*;
import java.util.stream.Collectors;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.elementary.EquipmentModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkStoreListener implements NetworkListener {

    private UUID groupUuid;

    private Network network;

    private final NetworkModificationRepository modificationRepository;

    private List<EquipmentModificationEntity> modifications = new LinkedList<>();

    Network getNetwork() {
        return network;
    }

    public static NetworkStoreListener create(Network network, UUID groupUuid, NetworkModificationRepository modificationRepository) {
        var listener = new NetworkStoreListener(network, groupUuid, modificationRepository);
        network.addListener(listener);
        return listener;
    }

    protected NetworkStoreListener(Network network, UUID groupUuid, NetworkModificationRepository modificationRepository) {
        this.network = network;
        this.groupUuid = groupUuid;
        this.modificationRepository = modificationRepository;
    }

    public List<EquipmenModificationInfos> getModifications() {
        return modifications.stream()
                .map(m -> m.toEquipmentModificationInfos(getSubstationIds(m.getEquipmentId())))
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

    private Set<String> getSubstationIds(String equipmentId) {
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
        // empty default implementation
    }

    @Override
    public void onRemoval(Identifiable identifiable) {
        // empty default implementation
    }
}
