 /**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.simple;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.SimpleModificationEntity;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "dataType",
    include = JsonTypeInfo.As.EXISTING_PROPERTY)
@JsonSubTypes({
    @JsonSubTypes.Type(value = StringSimpleModificationByFilterInfos.class, name = "STRING"),
    @JsonSubTypes.Type(value = BooleanSimpleModificationByFilterInfos.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumSimpleModificationByFilterInfos.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleSimpleModificationByFilterInfos.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerSimpleModificationByFilterInfos.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertySimpleModificationByFilterInfos.class, name = "PROPERTY"),
})
@JsonInclude(JsonInclude.Include.NON_NULL)
@SuperBuilder
@NoArgsConstructor
@Data
public class SimpleModificationByFilterInfos<T> extends AbstractModificationByFilterInfos {
    @Schema(description = "Data type")
    private DataType dataType;

    @JsonIgnore
    public SimpleModificationEntity toEntity() {
        throw new UnsupportedOperationException("Unsupported operation toEntity for instant of class " + this.getClass().getName());
    }

    public T getValue() {
        throw new UnsupportedOperationException("Unsupported operation getValue for instant of class " + this.getClass().getName());
    }
}
