/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.InjectionModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class InjectionModificationEntity extends BasicEquipmentModificationEntity {
    @Column(name = "voltageLevelIdValue")
    private String voltageLevelIdValue;

    @Column(name = "voltageLevelIdOp")
    @Enumerated(EnumType.STRING)
    private OperationType voltageLevelIdOp;

    @Column(name = "busOrBusbarSectionIdValue")
    private String busOrBusbarSectionIdValue;

    @Column(name = "busOrBusbarSectionIdOp")
    @Enumerated(EnumType.STRING)
    private OperationType busOrBusbarSectionIdOp;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionName")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionNameOp"))
    })
    private StringModificationEmbedded connectionName;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionDirection")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionDirectionOp"))
    })
    private EnumModificationEmbedded<ConnectablePosition.Direction> connectionDirection;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connectionPosition")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectionPositionOp"))
    })
    private IntegerModificationEmbedded connectionPosition;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "connected")),
        @AttributeOverride(name = "opType", column = @Column(name = "connectedOp"))
    })
    private BooleanModificationEmbedded terminalConnected;

    protected InjectionModificationEntity(InjectionModificationInfos modificationInfos) {
        super(modificationInfos);
        assignAttributes(modificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((InjectionModificationInfos) modificationInfos);
    }

    private void assignAttributes(InjectionModificationInfos modificationInfos) {
        this.voltageLevelIdValue = modificationInfos.getVoltageLevelId() != null ? modificationInfos.getVoltageLevelId().getValue() : null;
        this.voltageLevelIdOp = modificationInfos.getVoltageLevelId() != null ? modificationInfos.getVoltageLevelId().getOp() : null;
        this.busOrBusbarSectionIdValue = modificationInfos.getBusOrBusbarSectionId() != null ? modificationInfos.getBusOrBusbarSectionId().getValue() : null;
        this.busOrBusbarSectionIdOp = modificationInfos.getBusOrBusbarSectionId() != null ? modificationInfos.getBusOrBusbarSectionId().getOp() : null;
        this.connectionName = modificationInfos.getConnectionName() != null ? new StringModificationEmbedded(modificationInfos.getConnectionName()) : null;
        this.connectionDirection = modificationInfos.getConnectionDirection() != null ? new EnumModificationEmbedded<>(modificationInfos.getConnectionDirection()) : null;
        this.connectionPosition = modificationInfos.getConnectionPosition() != null ? new IntegerModificationEmbedded(modificationInfos.getConnectionPosition()) : null;
        this.terminalConnected = modificationInfos.getTerminalConnected() != null ? new BooleanModificationEmbedded(modificationInfos.getTerminalConnected()) : null;
    }
}
