/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.catalog;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.catalog.LimitsForLineTypeInfos;
import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;

import java.util.List;
import java.util.UUID;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Getter
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "lineTypesCatalog")
public class LineTypeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "type")
    private String type;

    @Column(name = "voltage")
    private Integer voltage;

    @Column(name = "conductorType")
    private String conductorType;

    @Column(name = "section")
    private Double section;

    @Column(name = "linearResistance")
    private Double linearResistance;

    @Column(name = "linearReactance")
    private Double linearReactance;

    @Column(name = "linearCapacity")
    private Double linearCapacity;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "line_type_id", nullable = false)
    private List<LimitsForLineTypeEntity> limitsForLineType;

    protected LineTypeEntity(LineTypeInfos lineType) {
        assignAttributes(lineType);
    }

    private void assignAttributes(LineTypeInfos lineType) {
        id = lineType.getId();
        type = lineType.getType();
        voltage = lineType.getVoltage();
        conductorType = lineType.getConductorType();
        section = lineType.getSection();
        linearResistance = lineType.getLinearResistance();
        linearReactance = lineType.getLinearReactance();
        linearCapacity = lineType.getLinearCapacity();
        if (lineType.getLimitsForLineType() != null) {
            limitsForLineType = lineType.getLimitsForLineType().stream().map(LimitsForLineTypeEntity::new).toList();
        }
    }

    LineTypeInfos.LineTypeInfosBuilder<?, ?> toBuilder() {
        return LineTypeInfos.builder()
            .id(this.id)
            .type(this.type)
            .voltage(this.voltage)
            .conductorType(this.conductorType)
            .section(this.section)
            .linearResistance(this.linearResistance)
            .linearReactance(this.linearReactance)
            .linearCapacity(this.linearCapacity);
    }

    public LineTypeInfos toDto() {
        return toBuilder().build();
    }

    public LineTypeInfos toDtoWithAreaAndTemperature() {
        return toBuilder()
                .limitsForLineType(this.limitsForLineType.parallelStream().map(LimitsForLineTypeEntity::toLineTypeInfosWithoutLimits).toList())
                .build();
    }

    public LineTypeInfos toDtoWithLimits(String area, String temperature) {
        List<LimitsForLineTypeInfos> test = this.limitsForLineType
                .stream()
                .filter(limitsForLineTypeEntity -> limitsForLineTypeEntity.getArea().equals(area) && limitsForLineTypeEntity.getTemperature().equals(temperature))
                .map(LimitsForLineTypeEntity::toLineTypeInfos).toList();
        return toBuilder()
            .limitsForLineType(test)
            .build();
    }
}

