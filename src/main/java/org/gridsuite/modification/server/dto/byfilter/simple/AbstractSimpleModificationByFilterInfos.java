 /**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
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
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.SimpleModificationEntity;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "dataType",
    include = JsonTypeInfo.As.EXISTING_PROPERTY)
@JsonSubTypes({
    @JsonSubTypes.Type(value = StringModificationByFilterInfos.class, name = "STRING"),
    @JsonSubTypes.Type(value = BooleanModificationByFilterInfos.class, name = "BOOLEAN"),
    @JsonSubTypes.Type(value = EnumModificationByFilterInfos.class, name = "ENUM"),
    @JsonSubTypes.Type(value = DoubleModificationByFilterInfos.class, name = "DOUBLE"),
    @JsonSubTypes.Type(value = IntegerModificationByFilterInfos.class, name = "INTEGER"),
    @JsonSubTypes.Type(value = PropertyModificationByFilterInfos.class, name = "PROPERTY"),
})
@JsonInclude(JsonInclude.Include.NON_NULL)
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public abstract class AbstractSimpleModificationByFilterInfos<T> extends AbstractModificationByFilterInfos {
    @Schema(description = "Value")
    private T value;

    public abstract DataType getDataType();

    @JsonIgnore
    public SimpleModificationEntity toEntity() {
        return new SimpleModificationEntity(this);
    }
}
