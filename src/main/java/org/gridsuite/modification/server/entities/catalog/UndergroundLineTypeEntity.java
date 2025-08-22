/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.catalog;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.catalog.UndergroundLineTypeInfos;

import java.util.List;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "undergroundLineTypesCatalog")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "undergroundLineType_id_fk_constraint"))
public class UndergroundLineTypeEntity extends LineTypeEntity {

    private static final List<Double> SHAPE_FACTORS = List.of(0.9d, 0.95d, 1d);

    @Column(name = "insulator")
    private String insulator;

    @Column(name = "screen")
    private String screen;

    public UndergroundLineTypeEntity(UndergroundLineTypeInfos undergroundLineType) {
        super(undergroundLineType);
        assignAttributes(undergroundLineType);
    }

    private void assignAttributes(UndergroundLineTypeInfos undergroundLineType) {
        insulator = undergroundLineType.getInsulator();
        screen = undergroundLineType.getScreen();
    }

    UndergroundLineTypeInfos.UndergroundLineTypeInfosBuilder<?, ?> toDtoBuilder() {
        return UndergroundLineTypeInfos.builder()
            .id(this.getId())
            .type(this.getType())
            .voltage(this.getVoltage())
            .conductorType(this.getConductorType())
            .section(this.getSection())
            .insulator(this.insulator)
            .screen(this.screen)
            .linearResistance(this.getLinearResistance())
            .linearReactance(this.getLinearReactance())
            .linearCapacity(this.getLinearCapacity());
    }

    @Override
    public UndergroundLineTypeInfos toDto() {
        return toDtoBuilder().build();
    }

    @Override
    public UndergroundLineTypeInfos toDtoWithLimits() {
        return toDtoBuilder()
            .shapeFactors(SHAPE_FACTORS)
            .limitsForLineType(this.getLimitsForLineType().stream().map(LimitsForLineTypeEntity::toLineTypeInfos).toList())
            .build();
    }
}


