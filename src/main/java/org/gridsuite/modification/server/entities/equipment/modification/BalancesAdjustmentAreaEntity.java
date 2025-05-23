/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.ShiftEquipmentType;
import org.gridsuite.modification.dto.ShiftType;
import org.gridsuite.modification.dto.BalancesAdjustmentAreaInfos;
import org.gridsuite.modification.server.utils.CountriesUtils;

import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Entity
@Builder
@Table(
    name = "balances_adjustment_area",
    indexes = {@Index(name = "area_balances_adjustment_id_idx", columnList = "balances_adjustment_id")}
)
public class BalancesAdjustmentAreaEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "countries")
    private String countries;

    @Column(name = "net_position")
    private Double netPosition;

    @Column(name = "shift_equipment_type")
    @Enumerated(EnumType.STRING)
    private ShiftEquipmentType shiftEquipmentType;

    @Column(name = "shift_type")
    @Enumerated(EnumType.STRING)
    private ShiftType shiftType;

    public static BalancesAdjustmentAreaInfos getAreaInfos(BalancesAdjustmentAreaEntity area) {
        return BalancesAdjustmentAreaInfos.builder()
            .name(area.getName())
            .countries(CountriesUtils.toList(area.getCountries()))
            .netPosition(area.getNetPosition())
            .shiftEquipmentType(area.getShiftEquipmentType())
            .shiftType(area.getShiftType())
            .build();
    }

    public static BalancesAdjustmentAreaEntity from(BalancesAdjustmentAreaInfos area) {
        return BalancesAdjustmentAreaEntity.builder()
            .name(area.getName())
            .countries(CountriesUtils.stringify(area.getCountries()))
            .netPosition(area.getNetPosition())
            .shiftEquipmentType(area.getShiftEquipmentType())
            .shiftType(area.getShiftType())
            .build();
    }
}
