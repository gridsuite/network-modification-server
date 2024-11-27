/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ShuntCompensatorCreationEmbeddable {
    @Column
    private String shuntCompensatorId;

    @Column
    private String shuntCompensatorName;

    @Column
    private Double maxQAtNominalV;

    @Column
    private Boolean connectedToHvdc;

    public static List<ShuntCompensatorCreationEmbeddable> toEmbeddableShuntCompensatorCreation(List<LccConverterStationCreationInfos.ShuntCompensatorInfos> compensatorCreationInfos) {
        return compensatorCreationInfos == null ? null :
                compensatorCreationInfos.stream()
                        .map(compensatorCreationInfo -> new ShuntCompensatorCreationEmbeddable(compensatorCreationInfo.getShuntCompensatorId(),
                                compensatorCreationInfo.getShuntCompensatorName(),
                                compensatorCreationInfo.getMaxQAtNominalV(),
                                compensatorCreationInfo.getConnectedToHvdc()))
                        .toList();
    }

    public static List<LccConverterStationCreationInfos.ShuntCompensatorInfos> fromEmbeddableShuntCompensatorCreation(List<ShuntCompensatorCreationEmbeddable> compensatorCreationEmbeddables) {
        return compensatorCreationEmbeddables == null ? null :
                compensatorCreationEmbeddables.stream()
                        .map(compensatorCreationEmbeddable -> new LccConverterStationCreationInfos.ShuntCompensatorInfos(compensatorCreationEmbeddable.getShuntCompensatorId(),
                                compensatorCreationEmbeddable.getShuntCompensatorName(),
                                compensatorCreationEmbeddable.getMaxQAtNominalV(),
                                compensatorCreationEmbeddable.getConnectedToHvdc()))
                        .toList();
    }
}
