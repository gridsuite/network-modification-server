/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.LoadType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;

import javax.persistence.*;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "loadModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "loadModification_id_fk_constraint"))
public class LoadModificationEntity extends InjectionModificationEntity {
    @Column(name = "loadTypeValue")
    private LoadType loadTypeValue;

    @Column(name = "loadTypeOp")
    private OperationType loadTypeOp;

    @Column(name = "activePowerValue")
    private Double activePowerValue;

    @Column(name = "activePowerOp")
    private OperationType activePowerOp;

    @Column(name = "reactivePowerValue")
    private Double reactivePowerValue;

    @Column(name = "reactivePowerOp")
    private OperationType reactivePowerOp;

    public LoadModificationEntity(String equipmentId, AttributeModification<String> equipmentName, AttributeModification<LoadType> loadType,
                              AttributeModification  voltageLevelId, AttributeModification<String> busOrBusbarSectionId,
                              AttributeModification<Double> activePower, AttributeModification<Double> reactivePower) {
        super(ModificationType.LOAD_MODIFICATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.loadTypeValue = loadType.getValue();
        this.loadTypeOp = loadType.getOp();
        this.activePowerValue = activePower.getValue();
        this.activePowerOp = activePower.getOp();
        this.reactivePowerValue = reactivePower.getValue();
        this.reactivePowerOp = reactivePower.getOp();
    }

    @Override
    public LoadModificationInfos toModificationInfos() {
        return toLoadModificationInfosBuilder().build();
    }

    private LoadModificationInfos.LoadModificationInfosBuilder<?, ?> toLoadModificationInfosBuilder() {
        return LoadModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(new AttributeModification<>(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(new AttributeModification<>(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(new AttributeModification<>(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .loadType(new AttributeModification<>(getLoadTypeValue(), getLoadTypeOp()))
                .activePower(new AttributeModification<>(getActivePowerValue(), getActivePowerOp()))
                .reactivePower(new AttributeModification<>(getReactivePowerValue(), getReactivePowerOp()));
    }
}
