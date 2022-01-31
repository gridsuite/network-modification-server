/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.gridsuite.modification.server.dto.TombstonedEquipmentInfos;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * A class to implement elasticsearch indexing
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
public class EquipmentInfosServiceImpl implements EquipmentInfosService {
    private final EquipmentInfosRepository equipmentInfosRepository;

    private final TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    @Value("${spring.data.elasticsearch.partition-size:10000}")
    private int partitionSize;

    public EquipmentInfosServiceImpl(EquipmentInfosRepository equipmentInfosRepository, TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository) {
        this.equipmentInfosRepository = equipmentInfosRepository;
        this.tombstonedEquipmentInfosRepository = tombstonedEquipmentInfosRepository;
    }

    @Override
    public EquipmentInfos addEquipmentInfos(@NonNull EquipmentInfos equipmentInfos) {
        return equipmentInfosRepository.save(equipmentInfos);
    }

    @Override
    public TombstonedEquipmentInfos addTombstonedEquipmentInfos(TombstonedEquipmentInfos tombstonedEquipmentInfos) {
        return tombstonedEquipmentInfosRepository.save(tombstonedEquipmentInfos);
    }

    @Override
    public void addAllEquipmentInfos(@NonNull final List<EquipmentInfos> equipmentsInfos) {
        Lists.partition(equipmentsInfos, partitionSize)
                .parallelStream()
                .forEach(equipmentInfosRepository::saveAll);
    }

    @Override
    public void addAllTombstonedEquipmentInfos(@NonNull final List<TombstonedEquipmentInfos> tombstonedEquipmentInfos) {
        Lists.partition(tombstonedEquipmentInfos, partitionSize)
                .parallelStream()
                .forEach(tombstonedEquipmentInfosRepository::saveAll);
    }

    @Override
    public void deleteEquipmentInfos(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId) {
        equipmentInfosRepository.deleteByIdAndNetworkUuidAndVariantId(equipmentId, networkUuid, variantId);
    }

    @Override
    public void deleteVariants(@NonNull UUID networkUuid, List<String> variantIds) {
        variantIds.forEach(variantId -> {
            equipmentInfosRepository.deleteAllByNetworkUuidAndVariantId(networkUuid, variantId);
            tombstonedEquipmentInfosRepository.deleteAllByNetworkUuidAndVariantId(networkUuid, variantId);
        });
    }

    @Override
    public void cloneVariantModifications(@NonNull UUID networkUuid, @NonNull String variantToCloneId, @NonNull String variantId) {
        addAllEquipmentInfos(
                StreamSupport.stream(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).spliterator(), false)
                        .map(equipmentInfos -> {
                            equipmentInfos.setUniqueId(null);
                            equipmentInfos.setVariantId(variantId);
                            return equipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
        addAllTombstonedEquipmentInfos(
                StreamSupport.stream(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).spliterator(), false)
                        .map(tombstonedEquipmentInfos -> {
                            tombstonedEquipmentInfos.setUniqueId(null);
                            tombstonedEquipmentInfos.setVariantId(variantId);
                            return tombstonedEquipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
    }

    @Override
    public boolean existEquipmentInfos(String equipmentId, UUID networkUuid, String variantId) {
        return Iterables.size(equipmentInfosRepository.findByIdAndNetworkUuidAndVariantId(equipmentId, networkUuid, variantId)) > 0;
    }

    @Override
    public List<EquipmentInfos> findAllEquipmentInfos(@NonNull UUID networkUuid) {
        return equipmentInfosRepository.findAllByNetworkUuid(networkUuid);
    }

    @Override
    public List<TombstonedEquipmentInfos> findAllTombstonedEquipmentInfos(UUID networkUuid) {
        return tombstonedEquipmentInfosRepository.findAllByNetworkUuid(networkUuid);
    }
}
