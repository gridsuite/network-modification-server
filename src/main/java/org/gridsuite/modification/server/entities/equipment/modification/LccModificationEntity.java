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
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.dto.LccModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "lcc_modification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lcc_modification_id_fk_constraint"))
public class LccModificationEntity extends BasicEquipmentModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "nominalv")),
        @AttributeOverride(name = "opType", column = @Column(name = "nominalv_op"))
    })
    private DoubleModificationEmbedded nominalV;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "r")),
        @AttributeOverride(name = "opType", column = @Column(name = "r_op"))
    })
    private DoubleModificationEmbedded r;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxp")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxp_op"))
    })
    private DoubleModificationEmbedded maxP;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "converters_mode")),
        @AttributeOverride(name = "opType", column = @Column(name = "converters_mode_op"))
    })
    private EnumModificationEmbedded<HvdcLine.ConvertersMode> convertersMode;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "active_power_setpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "active_power_setpoint_op"))
    })
    private DoubleModificationEmbedded activePowerSetpoint;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
        name = "lcc_converter_station_1_id",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "lcc_modification_converter_station_1_id_fk"
        ))
    private LccConverterStationModificationEntity converterStation1;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
        name = "lcc_converter_station_2_id",
        referencedColumnName = "id",
        foreignKey = @ForeignKey(
            name = "lcc_modification_converter_station_2_id_fk"
        ))
    private LccConverterStationModificationEntity converterStation2;

    public LccModificationEntity(@NonNull LccModificationInfos lccModificationInfos) {
        super(lccModificationInfos);
        assignAttributes(lccModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LccModificationInfos) modificationInfos);
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

    private LccModificationInfos.LccModificationInfosBuilder<?, ?> toLccModificationsInfosBuilder() {
        LccConverterStationModificationInfos converterStationInfos1 = getConverterStation1() == null ? null : getConverterStation1().toLccConverterStationInfos();
        LccConverterStationModificationInfos converterStationInfos2 = getConverterStation2() == null ? null : getConverterStation2().toLccConverterStationInfos();

        return LccModificationInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .nominalV(toAttributeModification(getNominalV()))
            .r(toAttributeModification(getR()))
            .maxP(toAttributeModification(getMaxP()))
            .activePowerSetpoint(toAttributeModification(getActivePowerSetpoint()))
            .convertersMode(toAttributeModification(getConvertersMode()))
            .converterStation1(converterStationInfos1)
            .converterStation2(converterStationInfos2)
            // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                getProperties().stream()
                    .map(FreePropertyEntity::toInfos)
                    .toList());
    }

    @Override
    public LccModificationInfos toModificationInfos() {
        return toLccModificationsInfosBuilder().build();
    }
}
