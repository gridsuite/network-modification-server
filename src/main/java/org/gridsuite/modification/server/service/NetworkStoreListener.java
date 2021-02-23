/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.NetworkListener;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.repositories.ModificationRepository;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
class NetworkStoreListener implements NetworkListener {

    private final ModificationRepository modificationRepository;

    static NetworkStoreListener create(Network network, ModificationRepository modificationRepository) {
        NetworkStoreListener listener = new NetworkStoreListener(modificationRepository);
        network.addListener(listener);
        return listener;
    }

    protected NetworkStoreListener(ModificationRepository modificationRepository) {
        this.modificationRepository = modificationRepository;
    }

    private List<ElementaryModificationInfos> modifications = new LinkedList<>();

    private void storeModification(Identifiable<?> identifiable, String attributeName, Object attributeValue) {
        ElementaryModificationEntity modificationEntity = new ElementaryModificationEntity(identifiable.getId(), createAttributeEntity(attributeName, attributeValue));
        modifications.add(this.modificationRepository.insert(modificationEntity).toElementaryModificationInfos());
    }

    private AbstractAttributeEntity createAttributeEntity(String attributeName, Object attributeValue) {
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
                return new BooleanAttributeEntity(attributeName, (boolean) attributeValue);
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
