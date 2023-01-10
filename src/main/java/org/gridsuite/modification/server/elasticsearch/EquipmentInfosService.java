/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import com.google.common.collect.Lists;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.TombstonedEquipmentInfos;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * A class to implement elasticsearch indexing
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */

@Service
public class EquipmentInfosService {
    private final EquipmentInfosRepository equipmentInfosRepository;

    private final TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    @Value("${spring.data.elasticsearch.partition-size:10000}")
    private int partitionSize;

    public EquipmentInfosService(@Lazy EquipmentInfosRepository equipmentInfosRepository, @Lazy TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository) {
        this.equipmentInfosRepository = equipmentInfosRepository;
        this.tombstonedEquipmentInfosRepository = tombstonedEquipmentInfosRepository;
    }

    public EquipmentInfos addEquipmentInfos(@NonNull EquipmentInfos equipmentInfos) {
        return equipmentInfosRepository.save(equipmentInfos);
    }

    public TombstonedEquipmentInfos addTombstonedEquipmentInfos(TombstonedEquipmentInfos tombstonedEquipmentInfos) {
        return tombstonedEquipmentInfosRepository.save(tombstonedEquipmentInfos);
    }

    public void addAllEquipmentInfos(@NonNull final List<EquipmentInfos> equipmentsInfos) {
        Lists.partition(equipmentsInfos, partitionSize)
                .parallelStream()
                .forEach(equipmentInfosRepository::saveAll);
    }

    public void addAllTombstonedEquipmentInfos(@NonNull final List<TombstonedEquipmentInfos> tombstonedEquipmentInfos) {
        Lists.partition(tombstonedEquipmentInfos, partitionSize)
                .parallelStream()
                .forEach(tombstonedEquipmentInfosRepository::saveAll);
    }

    public void deleteEquipmentInfos(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId) {
        equipmentInfosRepository.deleteByIdAndNetworkUuidAndVariantId(equipmentId, networkUuid, variantId);
    }

    public void deleteVariants(@NonNull UUID networkUuid, List<String> variantIds) {
        variantIds.forEach(variantId -> {
            equipmentInfosRepository.deleteAllByNetworkUuidAndVariantId(networkUuid, variantId);
            tombstonedEquipmentInfosRepository.deleteAllByNetworkUuidAndVariantId(networkUuid, variantId);
        });
    }

    public void cloneVariantModifications(@NonNull UUID networkUuid, @NonNull String variantToCloneId, @NonNull String variantId) {
        addAllEquipmentInfos(
                equipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).stream()
                        .map(equipmentInfos -> {
                            equipmentInfos.setUniqueId(null);
                            equipmentInfos.setVariantId(variantId);
                            return equipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
        addAllTombstonedEquipmentInfos(
                tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).stream()
                        .map(tombstonedEquipmentInfos -> {
                            tombstonedEquipmentInfos.setUniqueId(null);
                            tombstonedEquipmentInfos.setVariantId(variantId);
                            return tombstonedEquipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
    }

    public boolean existEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return equipmentInfosRepository.findByIdAndNetworkUuidAndVariantId(equipmentId, networkUuid, variantId).size() > 0;
    }

    public List<EquipmentInfos> findAllEquipmentInfos(@NonNull UUID networkUuid) {
        return equipmentInfosRepository.findAllByNetworkUuid(networkUuid);
    }

    public List<TombstonedEquipmentInfos> findAllTombstonedEquipmentInfos(UUID networkUuid) {
        return tombstonedEquipmentInfosRepository.findAllByNetworkUuid(networkUuid);
    }

    public boolean existTombstonedEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return !tombstonedEquipmentInfosRepository.findByIdAndNetworkUuidAndVariantId(equipmentId, networkUuid, variantId).isEmpty();
    }

    public void deleteAll() {
        equipmentInfosRepository.deleteAll();
        tombstonedEquipmentInfosRepository.deleteAll();
    }
}
