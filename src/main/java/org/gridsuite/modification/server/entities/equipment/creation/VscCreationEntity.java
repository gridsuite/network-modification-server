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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "vscCreation")
public class VscCreationEntity extends EquipmentCreationEntity {
    private Double nominalV;

    @Column
    private Double r;

    private Double maxP;

    @Column
    private Float operatorActivePowerLimitSide1;

    @Column
    private Float operatorActivePowerLimitSide2;

    @Column
    private HvdcLine.ConvertersMode convertersMode;

    @Column
    private Double activePowerSetpoint;

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
        this.activePowerSetpoint = vscCreationInfos.getActivePowerSetpoint();
        this.angleDroopActivePowerControl = vscCreationInfos.getAngleDroopActivePowerControl();
        this.droop = vscCreationInfos.getDroop();
        this.convertersMode = vscCreationInfos.getConvertersMode();
        this.nominalV = vscCreationInfos.getNominalV();
        this.r = vscCreationInfos.getR();
        this.operatorActivePowerLimitSide1 = vscCreationInfos.getOperatorActivePowerLimitFromSide1ToSide2();
        this.operatorActivePowerLimitSide2 = vscCreationInfos.getOperatorActivePowerLimitFromSide2ToSide1();
        this.maxP = vscCreationInfos.getMaxP();
        this.p0 = vscCreationInfos.getP0();
        this.converterStation1 = vscCreationInfos.getConverterStation1().toEntity();
        this.converterStation2 = vscCreationInfos.getConverterStation2().toEntity();
    }

    @Override
    public VscCreationInfos toModificationInfos() {
        return toVscCreationInfosBuilder().build();
    }

    private VscCreationInfos.VscCreationInfosBuilder<?, ?> toVscCreationInfosBuilder() {
        ConverterStationCreationInfos converterStationCreationInfos1 = getConverterStation1() == null ? null : getConverterStation1().toConverterStationInfos();
        ConverterStationCreationInfos converterStationCreationInfos2 = getConverterStation2() == null ? null : getConverterStation2().toConverterStationInfos();
        return VscCreationInfos.builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .activePowerSetpoint(getActivePowerSetpoint())
                .angleDroopActivePowerControl(getAngleDroopActivePowerControl())
                .droop(getDroop())
                .convertersMode(getConvertersMode())
                .nominalV(getNominalV())
                .r(getR())
                .operatorActivePowerLimitFromSide1ToSide2(getOperatorActivePowerLimitSide1())
                .operatorActivePowerLimitFromSide2ToSide1(getOperatorActivePowerLimitSide2())
                .maxP(getMaxP())
                .p0(getP0())
                .converterStation1(converterStationCreationInfos1)
                .converterStation2(converterStationCreationInfos2);
    }
}
