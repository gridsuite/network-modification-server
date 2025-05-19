/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.LccShuntCompensatorModificationInfos;

import java.util.List;

@NoArgsConstructor
@Getter
@Embeddable
public class ShuntCompensatorModificationEmbeddable extends AbstractShuntCompensatorEmbeddable {
    @Column(name = "deletion_mark")
    private Boolean deletionMark = false;

    ShuntCompensatorModificationEmbeddable(String id, String name, Double maxQAtNominalV, Boolean connectedToHvdc, Boolean deletionMark) {
        super(id, name, maxQAtNominalV, connectedToHvdc);
        this.deletionMark = deletionMark;
    }

    public static List<ShuntCompensatorModificationEmbeddable> toEmbeddableShuntCompensatorModification(List<LccShuntCompensatorModificationInfos> infos) {

        return infos == null ? null :
            infos.stream().map(modifInfo -> new ShuntCompensatorModificationEmbeddable(modifInfo.getId(),
                    modifInfo.getName(), modifInfo.getMaxQAtNominalV(), modifInfo.getConnectedToHvdc(), modifInfo.isDeletionMark()))
            .toList();
    }

    public static List<LccShuntCompensatorModificationInfos> fromEmbeddableShuntCompensatorModification(List<ShuntCompensatorModificationEmbeddable> compensatorModificationEmbeddables) {
        return compensatorModificationEmbeddables == null ? null :
            compensatorModificationEmbeddables.stream()
                .map(compensator ->
                    new LccShuntCompensatorModificationInfos(compensator.getId(),
                        compensator.getName(),
                        compensator.getMaxQAtNominalV(),
                        compensator.getConnectedToHvdc(),
                    compensator.getDeletionMark()))
                .toList();
    }

}
