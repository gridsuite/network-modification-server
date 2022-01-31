/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.TombstonedEquipmentInfos;
import org.springframework.lang.NonNull;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * A class to mock metadatas transfer in the DB elasticsearch
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
public class EquipmentInfosServiceMock implements EquipmentInfosService {

    @Override
    public EquipmentInfos addEquipmentInfos(@NonNull EquipmentInfos equipmentInfos) {
        return null;
    }

    @Override
    public TombstonedEquipmentInfos addTombstonedEquipmentInfos(TombstonedEquipmentInfos tombstonedEquipmentInfos) {
        return null;
    }

    @Override
    public void addAllEquipmentInfos(@NonNull final List<EquipmentInfos> equipmentInfos) {
        // Nothing to insert
    }

    @Override
    public void addAllTombstonedEquipmentInfos(List<TombstonedEquipmentInfos> tombstonedEquipmentsInfos) {
        // Nothing to insert
    }

    @Override
    public void deleteEquipmentInfos(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId) {
        // Nothing to delete
    }

    @Override
    public void deleteVariants(@NonNull UUID networkUuid, List<String> variantIds) {
        // Nothing to delete
    }

    @Override
    public void cloneVariantModifications(@NonNull UUID networkUuid, @NonNull String variantToCloneId, @NonNull String variantId) {
        // Do nothing
    }

    @Override
    public boolean existEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return false;
    }

    @Override
    public Iterable<EquipmentInfos> findAllEquipmentInfos(@NonNull UUID networkUuid) {
        return Collections.emptyList();
    }

    @Override
    public Iterable<TombstonedEquipmentInfos> findAllTombstonedEquipmentInfos(UUID networkUuid) {
        return Collections.emptyList();
    }
}
