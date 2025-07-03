/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.assignment;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.byfilter.DataType;
import org.gridsuite.modification.dto.byfilter.assignment.*;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.AbstractAssignmentEntity;

import javax.annotation.Nullable;
import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "assignment", indexes = @Index(name = "modification_by_assignment_id_idx", columnList = "modification_by_assignment_id"))
public class AssignmentEntity extends AbstractAssignmentEntity {
    @Column
    @Enumerated(EnumType.STRING)
    private DataType dataType;

    @Nullable
    @Column(name = "value_") // "value" is not supported in UT with H2
    private String value; // all values of different data types will be serialized as a string, deserialization is based on dataType

    @Setter
    @Column
    private String propertyName; // dedicated to an exceptional case, i.e. modify a property

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "assignment_id",
            foreignKey = @ForeignKey(name = "assignment_id_fk"))
    private List<VariationFilterEntity> filters;

    public AssignmentEntity(AssignmentInfos<?> assignmentInfos) {
        super(assignmentInfos);
        this.dataType = assignmentInfos.getDataType();
        this.value = assignmentInfos.getValue() == null ? null : assignmentInfos.getValue().toString();
        this.filters = assignmentInfos.getFilters().stream()
            .map(VariationFilterEntity::new)
            .toList();
        if (assignmentInfos instanceof PropertyAssignmentInfos propertyAssignmentInfos) {
            this.propertyName = propertyAssignmentInfos.getPropertyName();
        }
    }

    public AssignmentInfos<?> toAssignmentInfos() {
        AssignmentInfos<?> assignmentInfos = switch (dataType) {
            case BOOLEAN -> BooleanAssignmentInfos.builder()
                .value(Boolean.valueOf(value))
                .build();
            case INTEGER -> IntegerAssignmentInfos.builder()
                .value(Integer.valueOf(value))
                .build();
            case DOUBLE -> DoubleAssignmentInfos.builder()
                .value(Double.valueOf(value))
                .build();
            case ENUM -> EnumAssignmentInfos.builder()
                .value(value)
                .build();
            case PROPERTY -> PropertyAssignmentInfos.builder()
                .value(value)
                .propertyName(propertyName)
                .build();
            case STRING -> StringAssignmentInfos.builder()
                .value(value)
                .build();
        };

        assignAttributes(assignmentInfos);
        assignmentInfos.setFilters(filters.stream()
                .map(filterEntity -> new FilterInfos(filterEntity.getFilterId(), filterEntity.getName()))
                .toList());
        return assignmentInfos;
    }
}
