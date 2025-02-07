/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.google.common.collect.Iterables;
import com.powsybl.commons.extensions.Extension;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.Getter;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfosToDelete;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact.SimpleImpactType;

import java.util.*;
import java.util.stream.Stream;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFICATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkStoreListener implements NetworkListener {

    private final UUID networkUuid;

    @Getter
    private final Network network;

    private final NetworkStoreService networkStoreService;

    private final EquipmentInfosService equipmentInfosService;

    private final List<EquipmentInfosToDelete> deletedEquipments = new ArrayList<>();

    private final List<EquipmentInfos> createdEquipments = new ArrayList<>();

    private final List<EquipmentInfos> modifiedEquipments = new ArrayList<>();

    private final Set<SimpleElementImpact> simpleImpacts = new LinkedHashSet<>();

    private final Integer collectionThreshold;

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
    public void onPropertyRemoved(Identifiable identifiable, String attribute, Object oldValue) {
        addSimpleModificationImpact(identifiable);
    }

    @Override
    public void onPropertyAdded(Identifiable identifiable, String attribute, Object newValue) {
        addSimpleModificationImpact(identifiable);
    }

    @Override
    public void onPropertyReplaced(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        addSimpleModificationImpact(identifiable);
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
        deletedEquipments.add(new EquipmentInfosToDelete(identifiable.getId(), identifiable.getType().name()));
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

        return reduceNetworkImpacts();
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

        List<String> equipmentDeletionsIds = new ArrayList<>();
        List<TombstonedEquipmentInfos> tombstonedEquipmentInfos = new ArrayList<>();
        deletedEquipments.forEach(deletedEquipment -> {
            // add only allowed equipments types to be indexed to tombstonedEquipmentInfos
            if (!EquipmentInfosService.EXCLUDED_TYPES_FOR_INDEXING.contains(deletedEquipment.type())) {
                equipmentDeletionsIds.add(deletedEquipment.id());
                tombstonedEquipmentInfos.add(
                        TombstonedEquipmentInfos.builder()
                                .networkUuid(networkUuid)
                                .variantId(variantId)
                                .id(deletedEquipment.id())
                                .build());
            }
        });
        equipmentInfosService.deleteEquipmentInfosList(equipmentDeletionsIds, networkUuid, variantId);
        equipmentInfosService.addAllTombstonedEquipmentInfos(tombstonedEquipmentInfos);
        equipmentInfosService.addAllEquipmentInfos(createdEquipments);
        equipmentInfosService.addAllEquipmentInfos(modifiedEquipments);
    }

    private List<? extends AbstractBaseImpact> reduceDeletionImpacts() {
        List<SimpleElementImpact> deletionImpacts = getDeletionSimpleImpacts();
        return (getImpactedSubstationIds(deletionImpacts).size() >= collectionThreshold) ? getFullNetworkImpact() : deletionImpacts;
    }

    private List<AbstractBaseImpact> reduceNoDeletionImpacts() {
        List<AbstractBaseImpact> reducedImpacts = new ArrayList<>();
        Set<String> impactedSubstationsIds = new HashSet<>();

        // Group simple impacts over same element type into collection impact
        // And compute impactedSubstationsIds on the way
        for (IdentifiableType elementType : IdentifiableType.values()) {
            List<SimpleElementImpact> noDeletionImpactsByType = getNoDeletionSimpleImpacts(elementType);
            if (noDeletionImpactsByType.size() >= collectionThreshold) {
                reducedImpacts.add(CollectionElementImpact.builder()
                    .elementType(elementType)
                    .build());
            } else {
                impactedSubstationsIds.addAll(getImpactedSubstationIds(noDeletionImpactsByType));
            }
        }

        // All network is impacted ? then return only one substation collection impact
        if (impactedSubstationsIds.size() >= collectionThreshold) {
            return getFullNetworkImpact();
        }

        // Create simple impacts for substation type only
        reducedImpacts.addAll(
            impactedSubstationsIds.stream().map(id ->
                SimpleElementImpact.builder()
                    .simpleImpactType(SimpleImpactType.MODIFICATION)
                    .elementType(IdentifiableType.SUBSTATION)
                    .elementId(id)
                    .substationIds(Set.of(id))
                    .build()
            ).toList()
        );

        return reducedImpacts;
    }

    private List<AbstractBaseImpact> reduceNetworkImpacts() {

        List<? extends AbstractBaseImpact> reducedDeletionImpacts = reduceDeletionImpacts();
        if (isAllNetworkImpacted(reducedDeletionImpacts)) {
            return getFullNetworkImpact();
        }

        List<AbstractBaseImpact> reducedModificationImpacts = reduceNoDeletionImpacts();
        if (isAllNetworkImpacted(reducedModificationImpacts)) {
            return getFullNetworkImpact();
        }

        // fuse both reduced impacts
        return Stream.concat(reducedModificationImpacts.stream(), reducedDeletionImpacts.stream()).toList();
    }

    private List<AbstractBaseImpact> getFullNetworkImpact() {
        return List.of(CollectionElementImpact.builder()
                .elementType(IdentifiableType.SUBSTATION)
                .build());
    }

    private boolean isAllNetworkImpacted(List<? extends AbstractBaseImpact> impacts) {
        return impacts.stream()
            .filter(AbstractBaseImpact::isCollection)
            .filter(c -> c.getElementType() == IdentifiableType.SUBSTATION)
            .count() > 0;
    }

    private Set<String> getImpactedSubstationIds(List<SimpleElementImpact> impacts) {
        Set<String> impactedSubstationsIds = new HashSet<>();
        impactedSubstationsIds.addAll(impacts.stream().flatMap(i -> i.getSubstationIds().stream()).toList());
        return impactedSubstationsIds;
    }

    private List<SimpleElementImpact> getDeletionSimpleImpacts() {
        return simpleImpacts.stream()
                .filter(SimpleElementImpact::isDeletion)
                .distinct()
                .toList();
    }

    private List<SimpleElementImpact> getNoDeletionSimpleImpacts(IdentifiableType elementType) {
        return simpleImpacts.stream()
                .filter(i -> !i.isDeletion() && i.getElementType() == elementType)
                .distinct()
                .toList();
    }

    @Override
    public void onExtensionCreation(Extension<?> extension) {
        // FIXME: implement this method
    }

    @Override
    public void onExtensionAfterRemoval(Identifiable<?> identifiable, String extensionName) {
        // FIXME: implement this method
    }

    @Override
    public void onExtensionBeforeRemoval(Extension<?> extension) {
        // FIXME: implement this method
    }

    @Override
    public void onExtensionUpdate(Extension<?> extendable, String attribute, String variantId, Object oldValue, Object newValue) {
        Identifiable<?> identifiable = (Identifiable<?>) extendable.getExtendable();
        onUpdate(identifiable, attribute, variantId, oldValue, newValue);
    }

    @Override
    public void onVariantCreated(String sourceVariantId, String targetVariantId) {
        // FIXME: implement this method
    }

    @Override
    public void onVariantOverwritten(String sourceVariantId, String targetVariantId) {
        // FIXME: implement this method
    }

    @Override
    public void onVariantRemoved(String variantId) {
        // FIXME: implement this method
    }
}
