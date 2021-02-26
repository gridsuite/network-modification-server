/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;

import java.util.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkStoreListener implements NetworkListener {

    private UUID networkUuid;

    private final NetworkModificationRepository modificationRepository;

    private List<ElementaryModificationInfos> modifications = new LinkedList<>();

    public static NetworkStoreListener create(Network network, UUID networkUuid, NetworkModificationRepository modificationRepository) {
        NetworkStoreListener listener = new NetworkStoreListener(networkUuid, modificationRepository);
        network.addListener(listener);
        return listener;
    }

    protected NetworkStoreListener(UUID networkUuid, NetworkModificationRepository modificationRepository) {
        this.networkUuid = networkUuid;
        this.modificationRepository = modificationRepository;
    }

    private void storeModification(Identifiable<?> identifiable, String attributeName, Object attributeValue) {
        ElementaryModificationEntity modificationEntity = new ElementaryModificationEntity(identifiable.getId(), getSubstationIds(identifiable), createAttributeEntity(attributeName, attributeValue));
        modifications.add(this.modificationRepository.insertElementaryModification(networkUuid, modificationEntity).toElementaryModificationInfos());
    }

    private Set<String> getSubstationIds(Identifiable<?> identifiable) {
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

    private AbstractAttributeEntity createAttributeEntity(String attributeName, Object attributeValue) {
        if (attributeValue.getClass().isEnum()) {
            return new StringAttributeEntity(attributeName, attributeValue.toString());
        } else {
            switch (attributeValue.getClass().getSimpleName()) {
                case "String":
                    return new StringAttributeEntity(attributeName, (String) attributeValue);
                case "Boolean":
                    return new BooleanAttributeEntity(attributeName, (boolean) attributeValue);
                case "Integer":
                    return new IntegerAttributeEntity(attributeName, (int) attributeValue);
                case "Float":
                    return new FloatAttributeEntity(attributeName, (float) attributeValue);
                case "Double":
                    return new DoubleAttributeEntity(attributeName, (double) attributeValue);
                default:
                    throw new PowsyblException("Value type invalid : " + attributeValue.getClass().getSimpleName());
            }
        }
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        storeModification(identifiable, attribute, newValue);
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        storeModification(identifiable, attribute, newValue);
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        // empty default implementation
    }

    @Override
    public void onRemoval(Identifiable identifiable) {
        // empty default implementation
    }

    public List<ElementaryModificationInfos> getModifications() {
        return modifications;
    }
}
