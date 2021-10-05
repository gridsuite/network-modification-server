/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.lang.NonNull;

import java.util.UUID;

/**
 * A class to implement elasticsearch indexing
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class EquipmentInfosServiceImpl implements EquipmentInfosService {

    private static final Logger LOGGER = LoggerFactory.getLogger(EquipmentInfosServiceImpl.class);

    private final EquipmentInfosRepository equipmentInfosRepository;

    public EquipmentInfosServiceImpl(EquipmentInfosRepository equipmentInfosRepository) {
        this.equipmentInfosRepository = equipmentInfosRepository;
    }

    @Override
    public EquipmentInfos add(@NonNull EquipmentInfos equipmentInfos) {
        return equipmentInfosRepository.save(equipmentInfos);
    }

    @Override
    public void delete(@NonNull String equipmentId, @NonNull UUID networkUuid) {
        try {
            equipmentInfosRepository.deleteByEquipmentIdAndNetworkUuid(equipmentId, networkUuid);
        } catch (Exception e) {
            LOGGER.warn("Unable to remove elasticsearch index for equipment id " + equipmentId + " in network " + networkUuid);
        }
    }

    @Override
    public Iterable<EquipmentInfos> findAll(@NonNull UUID networkUuid) {
        return equipmentInfosRepository.findAllByNetworkUuid(networkUuid);
    }
}
