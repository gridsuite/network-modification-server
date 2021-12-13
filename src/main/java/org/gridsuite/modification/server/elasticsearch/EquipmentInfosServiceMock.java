/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.springframework.lang.NonNull;

import java.util.Collections;
import java.util.UUID;

/**
 * A class to mock metadatas transfer in the DB elasticsearch
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class EquipmentInfosServiceMock implements EquipmentInfosService {

    @Override
    public EquipmentInfos add(@NonNull EquipmentInfos equipmentInfos) {
        return null;
    }

    @Override
    public void delete(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId) {
        // Nothing to delete
    }

    @Override
    public boolean existEquipmentInVariant(String equipmentId, UUID networkUuid, String variantId) {
        return false;
    }

    @Override
    public Iterable<EquipmentInfos> findAll(@NonNull UUID networkUuid) {
        return Collections.emptyList();
    }
}
