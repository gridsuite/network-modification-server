/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.ElementaryModificationEntity;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Repository
public class ModificationRepository {

    private ElementaryModificationRepository elementaryModificationRepository;

    public ModificationRepository(ElementaryModificationRepository elementaryModificationRepository) {
        this.elementaryModificationRepository = elementaryModificationRepository;
    }

    public List<ElementaryModificationEntity> getElementaryModifications() {
        return this.elementaryModificationRepository.findAll();
    }

    public ElementaryModificationEntity insert(ElementaryModificationEntity elementaryModificationEntity) {
        return this.elementaryModificationRepository.save(elementaryModificationEntity);
    }

    public List<ElementaryModificationEntity> getElementaryModifications(String equipmentId) {
        return this.elementaryModificationRepository.findAllByEquipmentId(equipmentId);
    }

}
