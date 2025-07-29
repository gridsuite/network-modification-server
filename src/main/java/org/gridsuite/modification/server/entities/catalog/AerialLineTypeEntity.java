/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.catalog;

import lombok.NoArgsConstructor;

import org.gridsuite.modification.server.dto.DTOUtils;
import org.gridsuite.modification.server.dto.catalog.AerialLineTypeInfos;

import jakarta.persistence.*;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "aerialLineTypesCatalog")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "aerialLineType_id_fk_constraint"))
public class AerialLineTypeEntity extends LineTypeEntity {

    @Column(name = "conductorsNumber")
    private Integer conductorsNumber;

    @Column(name = "circuitsNumber")
    private Integer circuitsNumber;

    @Column(name = "groundWiresNumber")
    private Integer groundWiresNumber;

    public AerialLineTypeEntity(AerialLineTypeInfos aerialLineType) {
        super(aerialLineType);
        assignAttributes(aerialLineType);
    }

    private void assignAttributes(AerialLineTypeInfos aerialLineType) {
        conductorsNumber = aerialLineType.getConductorsNumber();
        circuitsNumber = aerialLineType.getCircuitsNumber();
        groundWiresNumber = aerialLineType.getGroundWiresNumber();
    }

    @Override
    public AerialLineTypeInfos toDto() {
        return AerialLineTypeInfos.builder()
                .id(this.getId())
                .type(this.getType())
                .voltage(this.getVoltage())
                .conductorType(this.getConductorType())
                .section(this.getSection())
                .conductorsNumber(this.conductorsNumber)
                .circuitsNumber(this.circuitsNumber)
                .groundWiresNumber(this.groundWiresNumber)
                .linearResistance(this.getLinearResistance())
                .linearReactance(this.getLinearReactance())
                .linearCapacity(this.getLinearCapacity())
                .limitsForLineType(DTOUtils.toLimitsForLineTypeInfos(this.getLimitsForLineType()))
                .build();
    }
}

