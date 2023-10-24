/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact.ImpactType;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
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

    private final Set<SimpleElementImpact> networkSimpleElementImpacts = new LinkedHashSet<>();

    private final Set<IdentifiableType> equipmentTypeCollectionImpacted = new LinkedHashSet<>();

    private Double collectionFactor;

    protected NetworkStoreListener(Network network, UUID networkUuid,
                                   NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService, Double collectionFactor) {
        this.network = network;
        this.networkUuid = networkUuid;
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.collectionFactor = collectionFactor;
    }

    public static NetworkStoreListener create(Network network, UUID networkUuid, NetworkStoreService networkStoreService,
                                              EquipmentInfosService equipmentInfosService, Double collectionFactor) {
        var listener = new NetworkStoreListener(network, networkUuid, networkStoreService, equipmentInfosService, collectionFactor);
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

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        addSimpleModificationImpact(identifiable);
    }

    private void addSimpleModificationImpact(Identifiable<?> identifiable) {
        addNetworkImpact(
                SimpleElementImpact.builder()
                        .impactType(ImpactType.MODIFICATION)
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
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        addSimpleModificationImpact(identifiable);
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
        addNetworkImpact(
            SimpleElementImpact.builder()
                .impactType(ImpactType.CREATION)
                .elementType(identifiable.getType())
                .elementId(identifiable.getId())
                .substationIds(getSubstationIds(identifiable))
                .build()
        );
    }

    @Override
    public void beforeRemoval(Identifiable identifiable) {
        deletedEquipmentsIds.add(identifiable.getId());
        addNetworkImpact(
            SimpleElementImpact.builder()
                .impactType(ImpactType.DELETION)
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

    public Set<AbstractBaseImpact> flushNetworkModifications() {
        try {
            networkStoreService.flush(network); // At first
            flushEquipmentInfos();

        } catch (Exception e) {
            throw new NetworkModificationException(MODIFICATION_ERROR, e);
        }

        // Append SimpleElement impacts to Collection Impacts
        Set<AbstractBaseImpact> impacts = new HashSet<>(networkSimpleElementImpacts.size() + equipmentTypeCollectionImpacted.size());
        impacts.addAll(networkSimpleElementImpacts);
        impacts.addAll(equipmentTypeCollectionImpacted.stream().map(type ->
            CollectionElementImpact.builder()
                .impactType(ImpactType.COLLECTION)
                .elementType(type)
                .build()
        ).collect(Collectors.toSet()));
        return impacts;
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

    private void addNetworkImpact(SimpleElementImpact impact) {

        // if there is already a Collection impact for this type then break
        if (equipmentTypeCollectionImpacted.contains(impact.getElementType())) {
            return;
        }
        // get number of SimpleElementimpacts on the same equipment type
        Set<SimpleElementImpact> typedNetworkImpacts = networkSimpleElementImpacts.stream().filter(i -> impact.getElementType() == i.getElementType()).collect(Collectors.toSet());
        long nbTypedImpacts = typedNetworkImpacts.size();
        // get number of this equipment type in the network
        int nbTypedEquipment = getEquipmentCount(impact.getElementType());
        // compare using a parameter (ex: if nbLoadImpacts >= 70% * nbLoads)
        if (nbTypedImpacts > nbTypedEquipment * collectionFactor) {
            // Mark this elementType as collection impacted
            equipmentTypeCollectionImpacted.add(impact.getElementType());
            // - remove impacts of this type, it will be replaced by a Collection impact
            networkSimpleElementImpacts.removeAll(typedNetworkImpacts);
        } else {
            // otherwise add the Simple Element impact
            networkSimpleElementImpacts.add(impact);
        }
    }

    private int getEquipmentCount(IdentifiableType type) {
        switch (type) {
            case BATTERY:
                return network.getBatteryCount();
            case BUS:
                // @ TODO return network.getBusCount(); ???
                return 0;
            case BUSBAR_SECTION:
                return network.getBusbarSectionCount();
            case DANGLING_LINE:
                return network.getDanglingLineCount();
            case GENERATOR:
                return network.getGeneratorCount();
            case HVDC_CONVERTER_STATION:
                return network.getHvdcConverterStationCount();
            case HVDC_LINE:
                return network.getHvdcLineCount();
            case LINE:
                return network.getLineCount();
            case LOAD:
                return network.getLoadCount();
            case SHUNT_COMPENSATOR:
                return network.getShuntCompensatorCount();
            case STATIC_VAR_COMPENSATOR:
                return network.getStaticVarCompensatorCount();
            case SUBSTATION:
                return network.getSubstationCount();
            case SWITCH:
                return network.getSwitchCount();
            case THREE_WINDINGS_TRANSFORMER:
                return network.getThreeWindingsTransformerCount();
            case TIE_LINE:
                return network.getTieLineCount();
            case TWO_WINDINGS_TRANSFORMER:
                return network.getTwoWindingsTransformerCount();
            case VOLTAGE_LEVEL:
                return network.getVoltageLevelCount();
            default:
                return 0;
        }
    }
}
