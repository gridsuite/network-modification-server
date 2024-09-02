/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.simple.*;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.ModificationByFilterEntity;

import java.util.List;
import java.util.Optional;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "simpleModification", indexes = @Index(name = "by_simple_modification_id_idx", columnList = "by_simple_modification_id"))
public class SimpleModificationEntity extends ModificationByFilterEntity {
    @Column
    @Enumerated(EnumType.STRING)
    private DataType dataType;

    @Column(name = "value_") // "value" is not supported in UT with H2
    private String value; // all values of different data types will be serialized as a string, deserialization is based on dataType

    @Setter
    @Column
    private String propertyName; // dedicated to an exceptional case, i.e. modify a property

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "simple_modification_id",
            foreignKey = @ForeignKey(name = "simple_modification_id_fk"))
    private List<VariationFilterEntity> filters;

    public SimpleModificationEntity(AbstractSimpleModificationByFilterInfos<?> simpleModificationInfos) {
        super(simpleModificationInfos);
        this.dataType = simpleModificationInfos.getDataType();
        this.value = Optional.ofNullable(simpleModificationInfos.getValue()).map(Object::toString).orElse(null);
        this.filters = simpleModificationInfos.getFilters().stream().map(FilterInfos::toEntity).toList();
    }

    public AbstractSimpleModificationByFilterInfos<?> toSimpleModificationInfos() {
        AbstractSimpleModificationByFilterInfos<?> simpleModificationByFilterInfos = switch (dataType) {
            case BOOLEAN -> BooleanModificationByFilterInfos.builder()
                .value(value != null ? Boolean.valueOf(value) : null)
                .build();
            case INTEGER -> IntegerModificationByFilterInfos.builder()
                .value(value != null ? Integer.valueOf(value) : null)
                .build();
            case DOUBLE -> DoubleModificationByFilterInfos.builder()
                .value(value != null ? Double.valueOf(value) : null)
                .build();
            case STRING -> StringModificationByFilterInfos.builder()
                .value(value)
                .build();
            case ENUM -> EnumModificationByFilterInfos.builder()
                .value(value)
                .build();
            case PROPERTY -> PropertyModificationByFilterInfos.builder()
                .value(value)
                .propertyName(propertyName)
                .build();
        };

        assignAttributes(simpleModificationByFilterInfos);
        simpleModificationByFilterInfos.setFilters(filters.stream()
                .map(filterEntity -> new FilterInfos(filterEntity.getFilterId(), filterEntity.getName()))
                .toList());
        return simpleModificationByFilterInfos;
    }
}
