/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import org.gridsuite.modification.server.LineKind;
import org.gridsuite.modification.server.dto.LineType;

import javax.persistence.*;
import java.util.UUID;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "line_type_catalog")
public class LineTypeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "kind")
    private LineKind kind;

    @Column(name = "type")
    private String type;

    @Column(name = "voltage")
    private Integer voltage;

    @Column(name = "conductorType")
    private String conductorType;

    @Column(name = "section")
    private Double section;

    @Column(name = "conductorsNumber")
    private Integer conductorsNumber;

    @Column(name = "circuitsNumber")
    private Integer circuitsNumber;

    @Column(name = "groundWiresNumber")
    private Integer groundWiresNumber;

    @Column(name = "linearResistance")
    private Double linearResistance;

    @Column(name = "linearReactance")
    private Double linearReactance;

    @Column(name = "linearCapacity")
    private Double linearCapacity;

    public LineTypeEntity(LineType lineType) {
        kind = lineType.getKind();
        type = lineType.getType();
        voltage = lineType.getVoltage();
        conductorType = lineType.getConductorType();
        section = lineType.getSection();
        conductorsNumber = lineType.getConductorsNumber();
        circuitsNumber = lineType.getCircuitsNumber();
        groundWiresNumber = lineType.getGroundWiresNumber();
        linearResistance = lineType.getLinearResistance();
        linearReactance = lineType.getLinearReactance();
        linearCapacity = lineType.getLinearCapacity();
    }

    public LineType toLineType() {
        return LineType.builder()
                .kind(this.kind)
                .type(this.type)
                .voltage(this.voltage)
                .conductorType(this.conductorType)
                .section(this.section)
                .conductorsNumber(this.conductorsNumber)
                .circuitsNumber(this.circuitsNumber)
                .groundWiresNumber(this.groundWiresNumber)
                .linearResistance(this.linearResistance)
                .linearReactance(this.linearReactance)
                .linearCapacity(this.linearCapacity)
                .build();
    }
}
