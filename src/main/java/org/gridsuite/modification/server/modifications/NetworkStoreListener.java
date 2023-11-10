/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.Setter;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkStoreListener implements NetworkListener {

    private final UUID networkUuid;

    private final Network network;

    private final NetworkStoreService networkStoreService;

    private final EquipmentInfosService equipmentInfosService;

    private final List<String> deletedEquipmentsIds = new ArrayList<>();

    private final List<EquipmentInfos> createdEquipments = new ArrayList<>();

    private final Set<SimpleElementImpact> networkImpacts = new LinkedHashSet<>();

    // TODO : Move to the NetworkModificationApplicator class
    @Setter
    private ApplicationStatus applicationStatus;
    @Setter
    private ApplicationStatus lastGroupApplicationStatus;

    protected NetworkStoreListener(Network network, UUID networkUuid,
                                   NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, NetworkStoreService networkStoreService,
                                              EquipmentInfosService equipmentInfosService) {
        var listener = new NetworkStoreListener(network, networkUuid, networkStoreService, equipmentInfosService);
        network.addListener(listener);
        return listener;
    }

    private static Set<String> getSubstationIds(Identifiable<?> identifiable) {
        Set<String> ids = new TreeSet<>();
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

    private void addSimpleModificationImpact(Identifiable<?> identifiable) {
        networkImpacts.add(
                SimpleElementImpact.builder()
                        .impactType(SimpleElementImpact.SimpleImpactType.MODIFICATION)
                        .elementType(identifiable.getType())
                        .elementId(identifiable.getId())
                        .substationIds(getSubstationIds(identifiable))
                        .build()
        );
    }

    @Override
    public void onElementRemoved(Identifiable identifiable, String attribute, Object oldValue) {
        addSimpleModificationImpact(identifiable);
    }

    @Override
    public void onElementAdded(Identifiable identifiable, String attribute, Object newValue) {
        addSimpleModificationImpact(identifiable);
    }

    @Override
    public void onElementReplaced(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        addSimpleModificationImpact(identifiable);
    }


    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        networkImpacts.add(
            SimpleElementImpact.builder()
                .impactType(SimpleElementImpact.SimpleImpactType.MODIFICATION)
                .elementType(identifiable.getType())
                .elementId(identifiable.getId())
                .substationIds(getSubstationIds(identifiable))
                .build()
        );
        equipmentInfosService.updateEquipment(identifiable, networkUuid, network.getVariantManager().getWorkingVariantId());
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        addSimpleModificationImpact(identifiable);
        equipmentInfosService.updateEquipment(identifiable, networkUuid, network.getVariantManager().getWorkingVariantId());
        // because all each equipment carry its linked voltage levels/substations name within its document
        // if attribute is "name" and identifiable type is VOLTAGE_LEVEL or SUBSTATION, we need to update all equipments linked to it
        if(attribute.equals("name") && (identifiable.getType().equals(IdentifiableType.VOLTAGE_LEVEL) || identifiable.getType().equals(IdentifiableType.SUBSTATION))) {
            equipmentInfosService.updateLinkedEquipments(identifiable, networkUuid, network.getVariantManager().getWorkingVariantId());
        }
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        createdEquipments.add(EquipmentInfos.builder()
            .networkUuid(networkUuid)
            .variantId(network.getVariantManager().getWorkingVariantId())
            .id(identifiable.getId())
            .name(identifiable.getNameOrId())
            .type(identifiable.getType().name())
            .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
            .substations(EquipmentInfos.getSubstationsInfos(identifiable))
            .build());
        networkImpacts.add(
            SimpleElementImpact.builder()
                .impactType(SimpleElementImpact.SimpleImpactType.CREATION)
                .elementType(identifiable.getType())
                .elementId(identifiable.getId())
                .substationIds(getSubstationIds(identifiable))
                .build()
        );
    }

    @Override
    public void beforeRemoval(Identifiable identifiable) {
        deletedEquipmentsIds.add(identifiable.getId());
        networkImpacts.add(
            SimpleElementImpact.builder()
                .impactType(SimpleElementImpact.SimpleImpactType.DELETION)
                .elementType(identifiable.getType())
                .elementId(identifiable.getId())
                .substationIds(getSubstationIds(identifiable))
                .build()
        );
    }

    @Override
    public void afterRemoval(String identifiableId) {
        // Do nothing
    }

    public NetworkModificationResult flushNetworkModifications() {
        try {
            networkStoreService.flush(network); // At first
            flushEquipmentInfos();

        } catch (Exception e) {
            throw new NetworkModificationException(MODIFICATION_ERROR, e);
        }

        // TODO : Move to the NetworkModificationApplicator class
        return
            NetworkModificationResult.builder()
                .applicationStatus(applicationStatus)
                .lastGroupApplicationStatus(lastGroupApplicationStatus)
                .networkImpacts(new ArrayList<>(networkImpacts))
                .build();
    }

    private void flushEquipmentInfos() {
        String variantId = network.getVariantManager().getWorkingVariantId();
        Set<String> presentEquipmentDeletionsIds = equipmentInfosService.findEquipmentInfosList(deletedEquipmentsIds, networkUuid, variantId).stream().map(EquipmentInfos::getId).collect(Collectors.toSet());

        List<String> equipmentDeletionsIds = new ArrayList<>();
        List<TombstonedEquipmentInfos> tombstonedEquipmentInfos = new ArrayList<>();
        deletedEquipmentsIds.forEach(id -> {
            if (presentEquipmentDeletionsIds.contains(id)) {
                equipmentDeletionsIds.add(id);
            } else {
                tombstonedEquipmentInfos.add(
                    TombstonedEquipmentInfos.builder()
                        .networkUuid(networkUuid)
                        .variantId(variantId)
                        .id(id)
                        .build());
            }
        });
        equipmentInfosService.deleteEquipmentInfosList(equipmentDeletionsIds, networkUuid, variantId);
        equipmentInfosService.addAllTombstonedEquipmentInfos(tombstonedEquipmentInfos);
        equipmentInfosService.addAllEquipmentInfos(createdEquipments);
    }
}
