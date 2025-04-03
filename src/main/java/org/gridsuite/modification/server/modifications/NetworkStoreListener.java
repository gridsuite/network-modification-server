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
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.dto.elasticsearch.BasicModificationInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfosToDelete;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.BasicModificationInfosService;
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

    private final BasicModificationInfosService basicModificationInfosService;

    private final List<BasicModificationInfos> impactedEquipmentsByModification = new LinkedList<>();

    private final Set<SimpleElementImpact> simpleImpacts = new LinkedHashSet<>();

    private final Integer collectionThreshold;

    protected NetworkStoreListener(Network network, UUID networkUuid,
                                   NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                   BasicModificationInfosService basicModificationInfosService, Integer collectionThreshold) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.basicModificationInfosService = basicModificationInfosService;
        this.collectionThreshold = collectionThreshold;
    }

    private void updateImpactedEquipment(Object impactedEquipment, SimpleImpactType impactType) {
        ImpactedEquipmentsInfos infosToUpdate = impactedEquipmentsByModification.getLast().getImpactedEquipmentsInfos();
        switch (impactType) {
            case CREATION -> infosToUpdate.getCreatedEquipments().add((EquipmentInfos) impactedEquipment);
            case MODIFICATION -> infosToUpdate.getModifiedEquipments().add((EquipmentInfos) impactedEquipment);
            case DELETION -> infosToUpdate.getDeletedEquipments().add((EquipmentInfosToDelete) impactedEquipment);
        }
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, NetworkStoreService networkStoreService,
                                              EquipmentInfosService equipmentInfosService, BasicModificationInfosService basicModificationInfosService, Integer collectionThreshold) {
        var listener = new NetworkStoreListener(network, networkUuid, networkStoreService, equipmentInfosService, basicModificationInfosService, collectionThreshold);
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
        updateImpactedEquipment(toEquipmentInfos(identifiable, networkUuid, variantId), SimpleImpactType.MODIFICATION);

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
                updateImpactedEquipment(toEquipmentInfos(linkedSubstation.get(), networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION);
            }
        } else if (identifiable.getType().equals(IdentifiableType.SUBSTATION)) {
            Substation updatedSubstation = network.getSubstation(identifiable.getId());
            updateEquipmentsLinkedToSubstation(updatedSubstation);
        }
    }

    private void updateEquipmentsLinkedToSubstation(Substation substation) {
        Iterable<VoltageLevel> linkedVoltageLevels = substation.getVoltageLevels();
        // update all voltageLevels linked to substation
        linkedVoltageLevels.forEach(vl -> updateImpactedEquipment(toEquipmentInfos(vl, networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION));
        // update all equipments linked to each of the voltageLevels
        linkedVoltageLevels.forEach(vl ->
            Iterables.concat(
                vl.getConnectables(),
                vl.getSwitches()
            ).forEach(c ->
                updateImpactedEquipment(toEquipmentInfos(c, networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION)
            )
        );
    }

    private void updateEquipmentsLinkedToVoltageLevel(VoltageLevel voltageLevel) {
        Iterables.concat(
            voltageLevel.getConnectables(),
            voltageLevel.getSwitches()
        ).forEach(c ->
            updateImpactedEquipment(toEquipmentInfos(c, networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION)
        );
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        updateImpactedEquipment(EquipmentInfos.builder()
            .networkUuid(networkUuid)
            .variantId(network.getVariantManager().getWorkingVariantId())
            .id(identifiable.getId())
            .name(identifiable.getNameOrId())
            .type(EquipmentInfos.getEquipmentTypeName(identifiable))
            .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
            .substations(EquipmentInfos.getSubstationsInfos(identifiable))
            .build(), SimpleImpactType.CREATION);

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
        updateImpactedEquipment(new EquipmentInfosToDelete(identifiable.getId(), identifiable.getType().name()), SimpleImpactType.DELETION);
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

    public void initNetworkModification(UUID groupUuid, UUID modificationUuid) {
        BasicModificationInfos modificationApplication = BasicModificationInfos.builder()
            .groupUuid(groupUuid)
            .modificationUuid(modificationUuid)
            .networkUuid(networkUuid)
            .build();
        impactedEquipmentsByModification.add(modificationApplication);
    }

    public List<AbstractBaseImpact> flushNetworkModifications() {
        try {
            networkStoreService.flush(network); // At first
            flushImpactedEquipments();
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
            .type(EquipmentInfos.getEquipmentTypeName(identifiable))
            .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
            .substations(EquipmentInfos.getSubstationsInfos(identifiable))
            .build();
    }

    private void flushImpactedEquipments() {
        flushDeletedEquipments();
        equipmentInfosService.addAllEquipmentInfos(CollectionUtils.union(getAllCreatedEquipments(), getAllModifiedEquipments()).stream().toList());
        basicModificationInfosService.addAll(impactedEquipmentsByModification.stream().map(BasicModificationInfos::flushImpactedEquipments).toList());
    }

    private void flushDeletedEquipments() {
        String variantId = network.getVariantManager().getWorkingVariantId();
        List<String> equipmentDeletionsIds = new ArrayList<>();
        List<TombstonedEquipmentInfos> tombstonedEquipmentInfos = new ArrayList<>();
        getAllDeletedEquipments().forEach(deletedEquipment -> {
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
    }

    private List<AbstractBaseImpact> reduceNetworkImpacts() {
        List<AbstractBaseImpact> reducedImpacts = new ArrayList<>();
        Set<String> impactedSubstationsIds = new HashSet<>();
        List<SimpleElementImpact> deletionImpacts = getDeletionSimpleImpacts();

        // All network is impacted by deletions
        if (getImpactedSubstationIds(deletionImpacts).size() >= collectionThreshold) {
            return getFullNetworkImpact();
        }

        // Group simple impacts over same element type into collection impact
        // And compute impactedSubstationsIds on the way
        for (IdentifiableType elementType : IdentifiableType.values()) {
            List<SimpleElementImpact> impactsByType = getCreationModificationSimpleImpacts(elementType);
            if (impactsByType.size() >= collectionThreshold) {
                reducedImpacts.add(CollectionElementImpact.builder()
                    .elementType(elementType)
                    .build());
            } else {
                impactedSubstationsIds.addAll(getImpactedSubstationIds(impactsByType));
            }
        }

        // All network is impacted by modifications and/or creations
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

        // Fuse both reduced impacts
        return Stream.concat(reducedImpacts.stream(), deletionImpacts.stream()).toList();
    }

    private List<AbstractBaseImpact> getFullNetworkImpact() {
        return List.of(CollectionElementImpact.builder()
                .elementType(IdentifiableType.SUBSTATION)
                .build());
    }

    private Set<String> getImpactedSubstationIds(List<SimpleElementImpact> impacts) {
        return new HashSet<>(impacts.stream().flatMap(i -> i.getSubstationIds().stream()).toList());
    }

    private List<SimpleElementImpact> getDeletionSimpleImpacts() {
        return simpleImpacts.stream()
                .filter(SimpleElementImpact::isDeletion)
                .distinct()
                .toList();
    }

    private List<SimpleElementImpact> getCreationModificationSimpleImpacts(IdentifiableType elementType) {
        return simpleImpacts.stream()
                .filter(i -> !i.isDeletion() && i.getElementType() == elementType)
                .distinct()
                .toList();
    }

    private List<EquipmentInfos> getAllCreatedEquipments() {
        return impactedEquipmentsByModification.stream().map(BasicModificationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getCreatedEquipments).flatMap(List::stream).toList();
    }

    private List<EquipmentInfos> getAllModifiedEquipments() {
        return impactedEquipmentsByModification.stream().map(BasicModificationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getModifiedEquipments).flatMap(List::stream).toList();
    }

    private List<EquipmentInfosToDelete> getAllDeletedEquipments() {
        return impactedEquipmentsByModification.stream().map(BasicModificationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getDeletedEquipments).flatMap(List::stream).toList();
    }

    @Override
    public void onExtensionCreation(Extension<?> extension) {
        Identifiable<?> identifiable = (Identifiable<?>) extension.getExtendable();
        addSimpleModificationImpact(identifiable);
        updateImpactedEquipment(toEquipmentInfos(identifiable, networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION);
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
    public void onExtensionUpdate(Extension<?> extension, String attribute, String variantId, Object oldValue, Object newValue) {
        Identifiable<?> identifiable = (Identifiable<?>) extension.getExtendable();
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
