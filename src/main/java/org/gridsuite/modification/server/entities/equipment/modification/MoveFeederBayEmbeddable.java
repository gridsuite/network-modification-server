/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.MoveFeederBayInfos;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class MoveFeederBayEmbeddable {
    @Column(name = "equipment_id")
    private String equipmentId;

    @Column(name = "busbar_section_id")
    private String busbarSectionId;

    @Schema(description = "connection_side")
    private String connectionSide;

    @Schema(description = "connection_position")
    private Integer connectionPosition;

    @Schema(description = "connection_name")
    private String connectionName;

    @Schema(description = "connection_direction")
    private ConnectablePosition.Direction connectionDirection;

    MoveFeederBayInfos toConnectablePositionModificationInfos() {
        return MoveFeederBayInfos.builder()
            .equipmentId(equipmentId)
            .busbarSectionId(busbarSectionId)
            .connectionSide(connectionSide)
            .connectionPosition(connectionPosition)
            .connectionName(connectionName)
            .connectionDirection(connectionDirection)
            .build();
    }

    static MoveFeederBayEmbeddable toConnectablePositionModificationEmbeddable(MoveFeederBayInfos moveFeederBayInfos) {
        return new MoveFeederBayEmbeddable(moveFeederBayInfos.getEquipmentId(),
            moveFeederBayInfos.getBusbarSectionId(),
            moveFeederBayInfos.getConnectionSide(),
            moveFeederBayInfos.getConnectionPosition(),
            moveFeederBayInfos.getConnectionName(),
            moveFeederBayInfos.getConnectionDirection());
    }
}
