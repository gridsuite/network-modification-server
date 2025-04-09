/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.extensions.Extension;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.Getter;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.dto.elasticsearch.BasicEquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact.SimpleImpactType;

import java.util.*;
import java.util.stream.Stream;

import static org.gridsuite.modification.NetworkModificationException.Type.MODIFICATION_ERROR;
import static org.gridsuite.modification.server.elasticsearch.EquipmentInfosService.TYPES_FOR_INDEXING;

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

    private final ModificationApplicationInfosService modificationApplicationInfosService;

    private final List<ModificationApplicationInfos> modificationApplicationInfosList = new LinkedList<>();

    private final Set<SimpleElementImpact> simpleImpacts = new LinkedHashSet<>();

    private final Integer collectionThreshold;

    protected NetworkStoreListener(Network network, UUID networkUuid,
                                   NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService,
                                   ModificationApplicationInfosService modificationApplicationInfosService, Integer collectionThreshold) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.modificationApplicationInfosService = modificationApplicationInfosService;
        this.collectionThreshold = collectionThreshold;
    }

    private void updateImpactedEquipment(BasicEquipmentInfos impactedEquipment, SimpleImpactType impactType) {
        ImpactedEquipmentsInfos infosToUpdate = modificationApplicationInfosList.getLast().getImpactedEquipmentsInfos();
        switch (impactType) {
            case CREATION -> infosToUpdate.getCreatedEquipments().add((EquipmentInfos) impactedEquipment);
            case MODIFICATION -> infosToUpdate.getModifiedEquipments().add((EquipmentInfos) impactedEquipment);
            case DELETION -> infosToUpdate.getTombstonedEquipments().add((TombstonedEquipmentInfos) impactedEquipment);
        }
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, NetworkStoreService networkStoreService,
                                              EquipmentInfosService equipmentInfosService, ModificationApplicationInfosService modificationApplicationInfosService, Integer collectionThreshold) {
        var listener = new NetworkStoreListener(network, networkUuid, networkStoreService, equipmentInfosService, modificationApplicationInfosService, collectionThreshold);
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
        if (TYPES_FOR_INDEXING.contains(identifiable.getType())) {
            updateEquipmentIndexation(identifiable, attribute, networkUuid, network.getVariantManager().getWorkingVariantId());
        }
    }

    private void updateEquipmentIndexation(Identifiable<?> identifiable, String attribute, UUID networkUuid, String variantId) {
        // Since only name changes impact indexation, we add the identifiable information to modifiedEquipments only in this case
        boolean shouldIndexEquipments = attribute.equals("name");
        updateImpactedEquipment(toEquipmentInfos(identifiable, networkUuid, variantId, shouldIndexEquipments), SimpleImpactType.MODIFICATION);
        // If the updated attribute is "name" and the identifiable is a VOLTAGE_LEVEL or SUBSTATION,
        // we must update all linked equipment to reflect the name change
        if (identifiable.getType().equals(IdentifiableType.VOLTAGE_LEVEL) || identifiable.getType().equals(IdentifiableType.SUBSTATION)) {
            updateLinkedEquipments(identifiable, shouldIndexEquipments);
        }
    }

    private void updateLinkedEquipments(Identifiable<?> identifiable, boolean shouldIndexEquipments) {
        if (identifiable.getType().equals(IdentifiableType.VOLTAGE_LEVEL)) {
            VoltageLevel updatedVoltageLevel = network.getVoltageLevel(identifiable.getId());
            // update all equipments linked to voltageLevel
            updateEquipmentsLinkedToVoltageLevel(updatedVoltageLevel);
            // update substation linked to voltageLevel
            Optional<Substation> linkedSubstation = updatedVoltageLevel.getSubstation();
            if (linkedSubstation.isPresent()) {
                updateImpactedEquipment(toEquipmentInfos(linkedSubstation.get(), networkUuid, network.getVariantManager().getWorkingVariantId(), shouldIndexEquipments), SimpleImpactType.MODIFICATION);
            }
        } else if (identifiable.getType().equals(IdentifiableType.SUBSTATION)) {
            Substation updatedSubstation = network.getSubstation(identifiable.getId());
            updateEquipmentsLinkedToSubstation(updatedSubstation, shouldIndexEquipments);
        }
    }

    private void updateEquipmentsLinkedToSubstation(Substation substation, boolean shouldIndexEquipments) {
        Iterable<VoltageLevel> linkedVoltageLevels = substation.getVoltageLevels();
        // update all voltageLevels linked to substation
        linkedVoltageLevels.forEach(vl -> updateImpactedEquipment(toEquipmentInfos(vl, networkUuid, network.getVariantManager().getWorkingVariantId(), shouldIndexEquipments), SimpleImpactType.MODIFICATION));
        // update all equipments linked to each of the voltageLevels
        linkedVoltageLevels.forEach(this::updateEquipmentsLinkedToVoltageLevel);
    }

    private void updateEquipmentsLinkedToVoltageLevel(VoltageLevel voltageLevel) {
        voltageLevel.getConnectableStream()
            .filter(connectable -> TYPES_FOR_INDEXING.contains(connectable.getType()))
            .forEach(c -> updateImpactedEquipment(toEquipmentInfos(c, networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION));
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        if (TYPES_FOR_INDEXING.contains(identifiable.getType())) {
            updateImpactedEquipment(EquipmentInfos.builder()
                .networkUuid(networkUuid)
                .variantId(network.getVariantManager().getWorkingVariantId())
                .id(identifiable.getId())
                .name(identifiable.getNameOrId())
                .type(EquipmentInfos.getEquipmentTypeName(identifiable))
                .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
                .substations(EquipmentInfos.getSubstationsInfos(identifiable))
                .build(), SimpleImpactType.CREATION);
        }
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
        if (TYPES_FOR_INDEXING.contains(identifiable.getType())) {
            updateImpactedEquipment(TombstonedEquipmentInfos.builder()
                .networkUuid(networkUuid)
                .variantId(network.getVariantManager().getWorkingVariantId())
                .id(identifiable.getId())
                .build(), SimpleImpactType.DELETION);
        }
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

    public void initModificationApplication(UUID groupUuid, ModificationEntity modification) {
        ModificationApplicationInfos modificationApplication = ModificationApplicationInfos.builder()
            .groupUuid(groupUuid)
            .modificationUuid(modification.getId())
            .networkUuid(networkUuid)
            .modification(modification)
            .build();
        modificationApplicationInfosList.add(modificationApplication);
    }

    public List<AbstractBaseImpact> flushModificationApplications() {
        try {
            networkStoreService.flush(network); // At first
            flushImpactedEquipments();
        } catch (Exception e) {
            throw new NetworkModificationException(MODIFICATION_ERROR, e);
        }

        return reduceNetworkImpacts();
    }

    private static EquipmentInfos toEquipmentInfos(Identifiable<?> identifiable, UUID networkUuid, String variantId) {
        return toEquipmentInfos(identifiable, networkUuid, variantId, true);
    }

    private static EquipmentInfos toEquipmentInfos(Identifiable<?> identifiable, UUID networkUuid, String variantId, boolean shouldIndexEquipments) {
        return EquipmentInfos.builder()
            .networkUuid(networkUuid)
            .variantId(variantId)
            .id(identifiable.getId())
            .name(identifiable.getNameOrId())
            .type(EquipmentInfos.getEquipmentTypeName(identifiable))
            .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
            .substations(EquipmentInfos.getSubstationsInfos(identifiable))
            .toBeIndexed(shouldIndexEquipments)
            .build();
    }

    private void flushImpactedEquipments() {
        flushDeletedEquipments();
        equipmentInfosService.addAllEquipmentInfos(CollectionUtils.union(getAllCreatedEquipments(), getAllModifiedEquipmentsToBeIndexed()).stream().toList());
        modificationApplicationInfosService.addAll(modificationApplicationInfosList.stream().map(ModificationApplicationInfos::flushImpactedEquipments).toList());
    }

    private void flushDeletedEquipments() {
        String variantId = network.getVariantManager().getWorkingVariantId();
        List<String> equipmentDeletionsIds = getAllTombstonedEquipments().stream().map(TombstonedEquipmentInfos::getId).toList();
        equipmentInfosService.deleteEquipmentInfosList(equipmentDeletionsIds, networkUuid, variantId);
        equipmentInfosService.addAllTombstonedEquipmentInfos(getAllTombstonedEquipments());
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
        return modificationApplicationInfosList.stream().map(ModificationApplicationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getCreatedEquipments).flatMap(List::stream).toList();
    }

    private List<EquipmentInfos> getAllModifiedEquipments() {
        return modificationApplicationInfosList.stream().map(ModificationApplicationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getModifiedEquipments).flatMap(List::stream).toList();
    }

    private List<TombstonedEquipmentInfos> getAllTombstonedEquipments() {
        return modificationApplicationInfosList.stream().map(ModificationApplicationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getTombstonedEquipments).flatMap(List::stream).toList();
    }

    private List<EquipmentInfos> getAllModifiedEquipmentsToBeIndexed() {
        return modificationApplicationInfosList.stream().map(ModificationApplicationInfos::getImpactedEquipmentsInfos).map(ImpactedEquipmentsInfos::getModifiedEquipments).flatMap(List::stream).filter(EquipmentInfos::isToBeIndexed).toList();
    }

    @Override
    public void onExtensionCreation(Extension<?> extension) {
        Identifiable<?> identifiable = (Identifiable<?>) extension.getExtendable();
        if (TYPES_FOR_INDEXING.contains(identifiable.getType())) {
            updateImpactedEquipment(toEquipmentInfos(identifiable, networkUuid, network.getVariantManager().getWorkingVariantId()), SimpleImpactType.MODIFICATION);
        }
        addSimpleModificationImpact(identifiable);
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
