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
import org.gridsuite.modification.model.FilterModel;
import org.gridsuite.modification.model.byfilter.DataType;
import org.gridsuite.modification.model.byfilter.assignment.*;
import org.gridsuite.modification.model.byfilter.assignment.AssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.BooleanAssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.DoubleAssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.EnumAssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.IntegerAssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.PropertyAssignmentModel;
import org.gridsuite.modification.model.byfilter.assignment.StringAssignmentModel;
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

    public AssignmentEntity(AssignmentModel<?> assignmentInfos) {
        super(assignmentInfos);
        this.dataType = assignmentInfos.getDataType();
        this.value = assignmentInfos.getValue() == null ? null : assignmentInfos.getValue().toString();
        this.filters = assignmentInfos.getFilters().stream()
            .map(VariationFilterEntity::new)
            .toList();
        if (assignmentInfos instanceof PropertyAssignmentModel propertyAssignmentInfos) {
            this.propertyName = propertyAssignmentInfos.getPropertyName();
        }
    }

    public AssignmentModel<?> toAssignmentInfos() {
        AssignmentModel<?> assignmentInfos = switch (dataType) {
            case BOOLEAN -> BooleanAssignmentModel.builder()
                .value(Boolean.valueOf(value))
                .build();
            case INTEGER -> IntegerAssignmentModel.builder()
                .value(value != null ? Integer.valueOf(value) : null)
                .build();
            case DOUBLE -> DoubleAssignmentModel.builder()
                .value(value != null ? Double.valueOf(value) : null)
                .build();
            case ENUM -> EnumAssignmentModel.builder()
                .value(value)
                .build();
            case PROPERTY -> PropertyAssignmentModel.builder()
                .value(value)
                .propertyName(propertyName)
                .build();
            case STRING -> StringAssignmentModel.builder()
                .value(value)
                .build();
        };

        assignAttributes(assignmentInfos);
        assignmentInfos.setFilters(filters.stream()
                .map(filterEntity -> new FilterModel(filterEntity.getFilterId(), filterEntity.getName()))
                .toList());
        return assignmentInfos;
    }
}
