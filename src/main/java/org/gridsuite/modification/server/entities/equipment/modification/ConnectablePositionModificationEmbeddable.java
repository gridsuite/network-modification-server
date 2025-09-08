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
import org.gridsuite.modification.dto.ConnectablePositionModificationInfos;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ConnectablePositionModificationEmbeddable {
    @Column(name = "equipmentId")
    private String equipmentId;

    @Column(name = "busbarSectionId")
    private String busbarSectionId;

    @Schema(description = "connectionSide")
    private String connectionSide;

    @Schema(description = "connectionPosition")
    private Integer connectionPosition;

    @Schema(description = "connectionName")
    private String connectionName;

    @Schema(description = "connectionDirection")
    private ConnectablePosition.Direction connectionDirection;

    ConnectablePositionModificationInfos toConnectablePositionModificationInfos() {
        return ConnectablePositionModificationInfos.builder()
            .equipmentId(equipmentId)
            .busbarSectionId(busbarSectionId)
            .connectionSide(connectionSide)
            .connectionPosition(connectionPosition)
            .connectionName(connectionName)
            .connectionDirection(connectionDirection)
            .build();
    }

    static ConnectablePositionModificationEmbeddable toConnectablePositionModificationEmbeddable(ConnectablePositionModificationInfos connectablePositionModificationInfos) {
        return new ConnectablePositionModificationEmbeddable(connectablePositionModificationInfos.getEquipmentId(),
            connectablePositionModificationInfos.getBusbarSectionId(),
            connectablePositionModificationInfos.getConnectionSide(),
            connectablePositionModificationInfos.getConnectionPosition(),
            connectablePositionModificationInfos.getConnectionName(),
            connectablePositionModificationInfos.getConnectionDirection());
    }
}
