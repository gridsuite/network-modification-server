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
import lombok.NonNull;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;

import jakarta.persistence.*;

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
    @Enumerated(EnumType.STRING)
    private OperationType loadTypeOp;

    @Column(name = "activePowerValue")
    private Double activePowerValue;

    @Column(name = "activePowerOp")
    @Enumerated(EnumType.STRING)
    private OperationType activePowerOp;

    @Column(name = "reactivePowerValue")
    private Double reactivePowerValue;

    @Column(name = "reactivePowerOp")
    @Enumerated(EnumType.STRING)
    private OperationType reactivePowerOp;

    public LoadModificationEntity(@NonNull LoadModificationInfos loadModificationInfos) {
        super(loadModificationInfos);
        assignAttributes(loadModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LoadModificationInfos) modificationInfos);
    }

    private void assignAttributes(LoadModificationInfos loadModificationInfos) {
        this.loadTypeValue = loadModificationInfos.getLoadType() != null ? loadModificationInfos.getLoadType().getValue() : null;
        this.loadTypeOp = loadModificationInfos.getLoadType() != null ? loadModificationInfos.getLoadType().getOp() : null;
        this.activePowerValue = loadModificationInfos.getActivePower() != null ? loadModificationInfos.getActivePower().getValue() : null;
        this.activePowerOp = loadModificationInfos.getActivePower() != null ? loadModificationInfos.getActivePower().getOp() : null;
        this.reactivePowerValue = loadModificationInfos.getReactivePower() != null ? loadModificationInfos.getReactivePower().getValue() : null;
        this.reactivePowerOp = loadModificationInfos.getReactivePower() != null ? loadModificationInfos.getReactivePower().getOp() : null;
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
                .stashed(getStashed())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .loadType(AttributeModification.toAttributeModification(getLoadTypeValue(), getLoadTypeOp()))
                .activePower(AttributeModification.toAttributeModification(getActivePowerValue(), getActivePowerOp()))
                .reactivePower(AttributeModification.toAttributeModification(getReactivePowerValue(), getReactivePowerOp()));
    }
}
