/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.HvdcLine;
import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "lccModification")
public class LccModificationEntity extends BasicEquipmentModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "nominalv")),
        @AttributeOverride(name = "opType", column = @Column(name = "nominalvOp"))
    })
    private DoubleModificationEmbedded nominalV;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "r")),
        @AttributeOverride(name = "opType", column = @Column(name = "rOp"))
    })
    private DoubleModificationEmbedded r;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxp")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxpOp"))
    })
    private DoubleModificationEmbedded maxP;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "convertersMode")),
        @AttributeOverride(name = "opType", column = @Column(name = "convertersModeOp"))
    })
    private EnumModificationEmbedded<HvdcLine.ConvertersMode> convertersMode;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "activePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "activePowerSetpointOp"))
    })
    private DoubleModificationEmbedded activePowerSetpoint;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
        name = "lcc_converter_station_1_id",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "lcc_converter_station_1_id_fk"
        ))
    private LccConverterStationModificationEntity converterStation1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
        name = "lcc_converter_station_2_id",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "lcc_converter_station_2_id_fk"
        ))
    private LccConverterStationModificationEntity converterStation2;

    public LccModificationEntity(@NonNull LccModificationInfos lccModificationInfos) {
        super(lccModificationInfos);
        assignAttributes(lccModificationInfos);
    }

    private void assignAttributes(@NonNull LccModificationInfos modificationInfos) {
        this.nominalV = new DoubleModificationEmbedded(modificationInfos.getNominalV());
        this.r = new DoubleModificationEmbedded(modificationInfos.getR());
        this.maxP = new DoubleModificationEmbedded(modificationInfos.getMaxP());
        this.activePowerSetpoint = new DoubleModificationEmbedded(modificationInfos.getActivePowerSetpoint());
        this.convertersMode = new EnumModificationEmbedded<>(modificationInfos.getConvertersMode());
        this.converterStation1 = new LccConverterStationModificationEntity(modificationInfos.getConverterStation1());
        this.converterStation2 = new LccConverterStationModificationEntity(modificationInfos.getConverterStation2());
    }

}
