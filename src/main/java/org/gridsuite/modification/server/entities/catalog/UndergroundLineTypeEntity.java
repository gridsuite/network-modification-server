/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.catalog;

import lombok.NoArgsConstructor;

import org.gridsuite.modification.server.dto.catalog.UndergroundLineTypeInfos;

import jakarta.persistence.*;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "undergroundLineTypesCatalog")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "undergroundLineType_id_fk_constraint"))
public class UndergroundLineTypeEntity extends LineTypeEntity {

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

    @Override
    public UndergroundLineTypeInfos toDto() {
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
                .linearCapacity(this.getLinearCapacity())
                .build();
    }
}


