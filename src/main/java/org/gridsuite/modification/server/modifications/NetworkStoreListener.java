/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.google.common.collect.Iterables;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact.SimpleImpactType;

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

    private final List<EquipmentInfos> modifiedEquipments = new ArrayList<>();

    private final Set<SimpleElementImpact> simpleImpacts = new LinkedHashSet<>();

    private Integer collectionThreshold;

    protected NetworkStoreListener(Network network, UUID networkUuid,
                                   NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService, Integer collectionThreshold) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.collectionThreshold = collectionThreshold;
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, NetworkStoreService networkStoreService,
                                              EquipmentInfosService equipmentInfosService, Integer collectionThreshold) {
        var listener = new NetworkStoreListener(network, networkUuid, networkStoreService, equipmentInfosService, collectionThreshold);
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
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeSides.ONE).getVoltageLevel().getSubstation().orElseThrow().getId());
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeSides.TWO).getVoltageLevel().getSubstation().orElseThrow().getId());
            ids.add(((ThreeWindingsTransformer) identifiable).getTerminal(ThreeSides.THREE).getVoltageLevel().getSubstation().orElseThrow().getId());
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
        simpleImpacts.add(
                SimpleElementImpact.builder()
                    .simpleImpactType(SimpleImpactType.MODIFICATION)
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
        addSimpleModificationImpact(identifiable);
        updateEquipmentIndexation(identifiable, attribute, networkUuid, network.getVariantManager().getWorkingVariantId());
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        addSimpleModificationImpact(identifiable);
        updateEquipmentIndexation(identifiable, attribute, networkUuid, network.getVariantManager().getWorkingVariantId());
    }

    private void updateEquipmentIndexation(Identifiable<?> identifiable, String attribute, UUID networkUuid, String variantId) {
        modifiedEquipments.add(toEquipmentInfos(identifiable, networkUuid, variantId));

        // because all each equipment carry its linked voltage levels/substations name within its document
        // if attribute is "name" and identifiable type is VOLTAGE_LEVEL or SUBSTATION, we need to update all equipments linked to it
        if (attribute.equals("name") && (identifiable.getType().equals(IdentifiableType.VOLTAGE_LEVEL) || identifiable.getType().equals(IdentifiableType.SUBSTATION))) {
            updateLinkedEquipments(identifiable);
        }
    }

    private void updateLinkedEquipments(Identifiable<?> identifiable) {
        if (identifiable.getType().equals(IdentifiableType.VOLTAGE_LEVEL)) {
            VoltageLevel updatedVoltageLevel = network.getVoltageLevel(identifiable.getId());
            // update all equipments linked to voltageLevel
            updateEquipmentsLinkedToVoltageLevel(updatedVoltageLevel);
            // update substation linked to voltageLevel
            Optional<Substation> linkedSubstation = updatedVoltageLevel.getSubstation();
            if (linkedSubstation.isPresent()) {
                modifiedEquipments.add(toEquipmentInfos(linkedSubstation.get(), networkUuid, network.getVariantManager().getWorkingVariantId()));
            }
        } else if (identifiable.getType().equals(IdentifiableType.SUBSTATION)) {
            Substation updatedSubstation = network.getSubstation(identifiable.getId());
            updateEquipmentsLinkedToSubstation(updatedSubstation);
        }
    }

    private void updateEquipmentsLinkedToSubstation(Substation substation) {
        Iterable<VoltageLevel> linkedVoltageLevels = substation.getVoltageLevels();
        // update all voltageLevels linked to substation
        linkedVoltageLevels.forEach(vl -> modifiedEquipments.add(toEquipmentInfos(vl, networkUuid, network.getVariantManager().getWorkingVariantId())));
        // update all equipments linked to each of the voltageLevels
        linkedVoltageLevels.forEach(vl ->
            Iterables.concat(
                vl.getConnectables(),
                vl.getSwitches()
            ).forEach(c ->
                modifiedEquipments.add(toEquipmentInfos(c, networkUuid, network.getVariantManager().getWorkingVariantId()))
            )
        );
    }

    private void updateEquipmentsLinkedToVoltageLevel(VoltageLevel voltageLevel) {
        Iterables.concat(
            voltageLevel.getConnectables(),
            voltageLevel.getSwitches()
        ).forEach(c ->
            modifiedEquipments.add(toEquipmentInfos(c, networkUuid, network.getVariantManager().getWorkingVariantId()))
        );
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
        simpleImpacts.add(
            SimpleElementImpact.builder()
                .simpleImpactType(SimpleImpactType.CREATION)
                .elementType(identifiable.getType())
                .elementId(identifiable.getId())
                .substationIds(getSubstationIds(identifiable))
                .build()
        );
    }

    @Override
    public void beforeRemoval(Identifiable identifiable) {
        deletedEquipmentsIds.add(identifiable.getId());
        simpleImpacts.add(
            SimpleElementImpact.builder()
                .simpleImpactType(SimpleImpactType.DELETION)
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

    public List<AbstractBaseImpact> flushNetworkModifications() {
        try {
            networkStoreService.flush(network); // At first
            flushEquipmentInfos();

        } catch (Exception e) {
            throw new NetworkModificationException(MODIFICATION_ERROR, e);
        }

        return flushNetworkImpacts();
    }

    private static EquipmentInfos toEquipmentInfos(Identifiable<?> identifiable, UUID networkUuid, String variantId) {
        return EquipmentInfos.builder()
            .networkUuid(networkUuid)
            .variantId(variantId)
            .id(identifiable.getId())
            .name(identifiable.getNameOrId())
            .type(identifiable.getType().name())
            .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
            .substations(EquipmentInfos.getSubstationsInfos(identifiable))
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
        equipmentInfosService.addAllEquipmentInfos(modifiedEquipments);
    }

    private List<AbstractBaseImpact> flushNetworkImpacts() {
        if (isAllNetworkImpacted()) {
            return List.of(CollectionElementImpact.builder()
                    .elementType(IdentifiableType.SUBSTATION)
                    .build());
        }

        return new ArrayList<>(reduceNetworkImpacts());
    }

    private boolean isAllNetworkImpacted() {
        Set<String> allImpactedSubstationsIds = simpleImpacts.stream().flatMap(i -> i.getSubstationIds().stream()).collect(Collectors.toSet());
        return allImpactedSubstationsIds.size() >= collectionThreshold;
    }

    private Set<AbstractBaseImpact> reduceNetworkImpacts() {
        Set<AbstractBaseImpact> reducedImpacts = new HashSet<>();
        Set<String> impactedSubstationsIds = new HashSet<>();

        // Impacts type collection
        Arrays.stream(IdentifiableType.values()).forEach(elementType -> {
            Set<SimpleElementImpact> impacts = getSimpleImpacts(elementType);
            if (impacts.size() >= collectionThreshold) {
                reducedImpacts.add(CollectionElementImpact.builder()
                    .elementType(elementType)
                    .build());
            } else {
                impactedSubstationsIds.addAll(impacts.stream().flatMap(i -> i.getSubstationIds().stream()).collect(Collectors.toSet()));
            }
        });

        // Impacts type simple for substation only
        reducedImpacts.addAll(impactedSubstationsIds.stream().map(id ->
            SimpleElementImpact.builder()
                .simpleImpactType(SimpleImpactType.MODIFICATION)
                .elementType(IdentifiableType.SUBSTATION)
                .elementId(id)
                .substationIds(Set.of(id))
                .build()
        ).collect(Collectors.toSet()));

        // Impacts type simple for deletion only
        reducedImpacts.addAll(simpleImpacts.stream().filter(i -> i.getSimpleImpactType() == SimpleImpactType.DELETION).collect(Collectors.toSet()));

        return reducedImpacts;
    }

    private Set<SimpleElementImpact> getSimpleImpacts(IdentifiableType elementType) {
        return simpleImpacts.stream()
                .filter(i -> i.getSimpleImpactType() != SimpleImpactType.DELETION)
                .filter(i -> i.getElementType() == elementType)
                .collect(Collectors.toSet());
    }

}
