/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

/**
 * An interface to define an api for metadatas transfer in the DB elasticsearch
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public interface EquipmentInfosService {
    EquipmentInfos add(@NonNull EquipmentInfos equipmentInfos);

    void addAll(@NonNull final List<EquipmentInfos> equipmentsInfos);

    void deleteEquipmentInVariant(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId);

    void deleteVariants(@NonNull UUID networkUuid, List<String> variantIds);

    void cloneVariantModifications(@NonNull UUID networkUuid, @NonNull String variantToCloneId, @NonNull String variantId);

    boolean existEquipmentInVariant(String equipmentId, UUID networkUuid, String variantId);

    Iterable<EquipmentInfos> findAll(@NonNull UUID networkUuid); // Only for tests
}
