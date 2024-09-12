 /**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.assignment;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.byfilter.AbstractAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.AssignmentEntity;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "dataType",
    include = JsonTypeInfo.As.EXISTING_PROPERTY)
@JsonSubTypes({
    @JsonSubTypes.Type(value = BooleanAssignmentInfos.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumAssignmentInfos.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleAssignmentInfos.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerAssignmentInfos.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertyAssignmentInfos.class, name = "PROPERTY"),
})
@JsonInclude(JsonInclude.Include.NON_NULL)
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class AssignmentInfos<T> extends AbstractAssignmentInfos {
    @Schema(description = "Value")
    private T value;

    public DataType getDataType() {
        throw new UnsupportedOperationException("This method should not be called");
    }

    @JsonIgnore
    public AssignmentEntity toEntity() {
        return new AssignmentEntity(this);
    }
}
