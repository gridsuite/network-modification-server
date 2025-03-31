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
import org.gridsuite.modification.dto.LccShuntCompensatorInfos;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ShuntCompensatorCreationEmbeddable {
    @Column(name = "shuntCompensatorId")
    private String id;

    @Column(name = "shuntCompensatorName")
    private String name;

    @Column
    private Double maxQAtNominalV;

    @Column
    private Boolean connectedToHvdc;

    public static List<ShuntCompensatorCreationEmbeddable> toEmbeddableShuntCompensatorCreation(List<LccShuntCompensatorInfos> compensatorCreationInfos) {
        return compensatorCreationInfos == null ? null :
                compensatorCreationInfos.stream()
                        .map(compensatorCreationInfo -> new ShuntCompensatorCreationEmbeddable(compensatorCreationInfo.getId(),
                                compensatorCreationInfo.getName(),
                                compensatorCreationInfo.getMaxQAtNominalV(),
                                compensatorCreationInfo.getConnectedToHvdc()))
                        .toList();
    }

    public static List<LccShuntCompensatorInfos> fromEmbeddableShuntCompensatorCreation(List<ShuntCompensatorCreationEmbeddable> compensatorCreationEmbeddables) {
        return compensatorCreationEmbeddables == null ? null :
                compensatorCreationEmbeddables.stream()
                        .map(compensatorCreationEmbeddable ->
                            new LccShuntCompensatorInfos(compensatorCreationEmbeddable.getId(),
                                compensatorCreationEmbeddable.getName(),
                                compensatorCreationEmbeddable.getMaxQAtNominalV(),
                                compensatorCreationEmbeddable.getConnectedToHvdc()))
                        .toList();
    }
}
