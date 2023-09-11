/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.HvdcLine;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VscCreationInfos;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "vscCreation")
public class VscCreationEntity extends EquipmentCreationEntity {
    @Column
    private Double dcNominalVoltage;

    @Column
    private Double dcResistance;

    @Column
    private Double maximumActivePower;

    @Column
    private Float operatorActivePowerLimitSide1;

    @Column
    private Float operatorActivePowerLimitSide2;

    @Column
    private HvdcLine.ConvertersMode convertersMode;

    @Column
    private Double activePower;

    @Column
    private Boolean angleDroopActivePowerControl;

    @Column
    private Float p0;

    @Column
    private Float droop;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "converter_station_1_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "converter_station_1_id_fk"
            ))
    private ConverterStationCreationEntity converterStation1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "converter_station_2_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "converter_station_2_id_fk"
            ))
    private ConverterStationCreationEntity converterStation2;

    public VscCreationEntity(@NonNull VscCreationInfos vscCreationInfos) {
        super(vscCreationInfos);
        assignAttributes(vscCreationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VscCreationInfos) modificationInfos);
    }

    private void assignAttributes(VscCreationInfos vscCreationInfos) {
        this.activePower = vscCreationInfos.getActivePower();
        this.angleDroopActivePowerControl = vscCreationInfos.getAngleDroopActivePowerControl();
        this.droop = vscCreationInfos.getDroop();
        this.convertersMode = vscCreationInfos.getConvertersMode();
        this.dcNominalVoltage = vscCreationInfos.getDcNominalVoltage();
        this.dcResistance = vscCreationInfos.getDcResistance();
        this.operatorActivePowerLimitSide1 = vscCreationInfos.getOperatorActivePowerLimitSide1();
        this.operatorActivePowerLimitSide2 = vscCreationInfos.getOperatorActivePowerLimitSide2();
        this.maximumActivePower = vscCreationInfos.getMaximumActivePower();
        this.p0 = vscCreationInfos.getP0();
        this.converterStation1 = new ConverterStationCreationEntity(vscCreationInfos.getConverterStation1());
        this.converterStation2 = new ConverterStationCreationEntity(vscCreationInfos.getConverterStation2());
    }

    @Override
    public VscCreationInfos toModificationInfos() {
        return toVscCreationInfosBuilder().build();
    }

    private VscCreationInfos.VscCreationInfosBuilder<?, ?> toVscCreationInfosBuilder() {
        ConverterStationCreationInfos converterStation1 = getConverterStation1() == null ? null : getConverterStation1().toConverterStationInfos();
        ConverterStationCreationInfos converterStation2 = getConverterStation2() == null ? null : getConverterStation2().toConverterStationInfos();

        return VscCreationInfos.builder()
                .activePower(getActivePower())
                .angleDroopActivePowerControl(getAngleDroopActivePowerControl())
                .droop(getDroop())
                .convertersMode(getConvertersMode())
                .dcNominalVoltage(getDcNominalVoltage())
                .dcResistance(getDcResistance())
                .operatorActivePowerLimitSide1(getOperatorActivePowerLimitSide1())
                .operatorActivePowerLimitSide2(getOperatorActivePowerLimitSide2())
                .maximumActivePower(getMaximumActivePower())
                .p0(getP0())
                .converterStation1(converterStation1)
                .converterStation2(converterStation2);
    }
}
