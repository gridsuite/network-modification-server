/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.MoveVoltageLevelFeederBaysInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "move_voltage_level_feeder_bays")
public class MoveVoltageLevelFeederBaysEntity extends ModificationEntity {

    @Column(name = "voltageLevelId")
    private String voltageLevelId;

    @ElementCollection
    @CollectionTable(
        name = "move_feeder_bay",
        joinColumns = @JoinColumn(name = "modification_id"),
            foreignKey = @ForeignKey(name = "move_feeder_bay_modification_id_fk_constraint")
    )
    private List<MoveFeederBayEmbeddable> moveFeederBays;

    public MoveVoltageLevelFeederBaysEntity(MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos) {
        super(moveVoltageLevelFeederBaysInfos);
        assignAttributes(moveVoltageLevelFeederBaysInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((MoveVoltageLevelFeederBaysInfos) modificationInfos);
    }

    private void assignAttributes(MoveVoltageLevelFeederBaysInfos moveVoltageLevelFeederBaysInfos) {
        if (this.moveFeederBays == null) {
            this.moveFeederBays = new ArrayList<>();
        } else {
            this.moveFeederBays.clear();
        }
        this.voltageLevelId = moveVoltageLevelFeederBaysInfos.getVoltageLevelId();
        this.moveFeederBays.addAll(moveVoltageLevelFeederBaysInfos.getFeederBays().stream()
            .map(MoveFeederBayEmbeddable::toConnectablePositionModificationEmbeddable)
            .toList());
    }

    @Override
    public MoveVoltageLevelFeederBaysInfos toModificationInfos() {
        return toMoveVoltageLevelFeederBaysInfosBuilder().build();
    }

    private MoveVoltageLevelFeederBaysInfos.MoveVoltageLevelFeederBaysInfosBuilder<?, ?> toMoveVoltageLevelFeederBaysInfosBuilder() {
        return MoveVoltageLevelFeederBaysInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .voltageLevelId(voltageLevelId)
            .feederBays(moveFeederBays.stream()
                .map(MoveFeederBayEmbeddable::toConnectablePositionModificationInfos).toList());
    }
}
