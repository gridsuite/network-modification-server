/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import com.google.common.collect.Lists;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
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

    @Value("${spring.data.elasticsearch.partition-size-for-deletion:2048}")
    public int partitionSizeForDeletion;

    public static final Set<String> TYPES_FOR_INDEXING = Set.of(
            IdentifiableType.SUBSTATION.name(),
            IdentifiableType.VOLTAGE_LEVEL.name(),
            IdentifiableType.HVDC_LINE.name(),
            IdentifiableType.LINE.name(),
            IdentifiableType.TIE_LINE.name(),
            IdentifiableType.TWO_WINDINGS_TRANSFORMER.name(),
            IdentifiableType.THREE_WINDINGS_TRANSFORMER.name(),
            IdentifiableType.GENERATOR.name(),
            IdentifiableType.BATTERY.name(),
            IdentifiableType.LOAD.name(),
            IdentifiableType.SHUNT_COMPENSATOR.name(),
            IdentifiableType.DANGLING_LINE.name(),
            IdentifiableType.STATIC_VAR_COMPENSATOR.name(),
            IdentifiableType.HVDC_CONVERTER_STATION.name());

    public EquipmentInfosService(EquipmentInfosRepository equipmentInfosRepository, TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository) {
        this.equipmentInfosRepository = equipmentInfosRepository;
        this.tombstonedEquipmentInfosRepository = tombstonedEquipmentInfosRepository;
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

    public void deleteEquipmentInfosList(@NonNull List<String> equipmentIds, @NonNull UUID networkUuid, @NonNull String variantId) {
        Lists.partition(equipmentIds, partitionSizeForDeletion)
                .forEach(ids -> equipmentInfosRepository.deleteByIdInAndNetworkUuidAndVariantId(ids, networkUuid, variantId));
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
                            equipmentInfos.setVariantId(variantId);
                            return equipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
        addAllTombstonedEquipmentInfos(
                tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).stream()
                        .map(tombstonedEquipmentInfos -> {
                            tombstonedEquipmentInfos.setVariantId(variantId);
                            return tombstonedEquipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
    }

    public void deleteAll() {
        equipmentInfosRepository.deleteAll();
        tombstonedEquipmentInfosRepository.deleteAll();
    }
}
