/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.HvdcLine;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.LccConverterStationCreationInfos;
import org.gridsuite.modification.dto.LccCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

/**
 * @author Rehili Ghazwa <ghazwa.rehili at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "lccCreation")
public class LccCreationEntity extends EquipmentCreationEntity {
    @Column
    private Double nominalV;

    @Column
    private Double r;

    @Column
    private Double maxP;

    @Column
    @Enumerated(EnumType.STRING)
    private HvdcLine.ConvertersMode convertersMode;

    @Column
    private Double activePowerSetpoint;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "lcc_converter_station_1_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "lcc_converter_station_1_id_fk"
            ))
    private LccConverterStationCreationEntity converterStation1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "lcc_converter_station_2_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "lcc_converter_station_2_id_fk"
            ))
    private LccConverterStationCreationEntity converterStation2;

    public LccCreationEntity(@NonNull LccCreationInfos lccCreationInfos) {
        super(lccCreationInfos);
        assignAttributes(lccCreationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LccCreationInfos) modificationInfos);
    }

    private void assignAttributes(LccCreationInfos lccCreationInfos) {
        this.nominalV = lccCreationInfos.getNominalV();
        this.r = lccCreationInfos.getR();
        this.maxP = lccCreationInfos.getMaxP();
        this.activePowerSetpoint = lccCreationInfos.getActivePowerSetpoint();
        this.convertersMode = lccCreationInfos.getConvertersMode();
        this.converterStation1 = new LccConverterStationCreationEntity(lccCreationInfos.getConverterStation1());
        this.converterStation2 = new LccConverterStationCreationEntity(lccCreationInfos.getConverterStation2());
    }

    @Override
    public LccCreationInfos toModificationInfos() {
        return toLccCreationInfosBuilder().build();
    }

    private LccCreationInfos.LccCreationInfosBuilder<?, ?> toLccCreationInfosBuilder() {
        LccConverterStationCreationInfos converterStationCreationInfos1 = getConverterStation1() == null ? null : getConverterStation1().toLccConverterStationInfos();
        LccConverterStationCreationInfos converterStationCreationInfos2 = getConverterStation2() == null ? null : getConverterStation2().toLccConverterStationInfos();
        return LccCreationInfos.builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .nominalV(getNominalV())
                .r(getR())
                .maxP(getMaxP())
                .activePowerSetpoint(getActivePowerSetpoint())
                .convertersMode(getConvertersMode())
                .converterStation1(converterStationCreationInfos1)
                .converterStation2(converterStationCreationInfos2)
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
