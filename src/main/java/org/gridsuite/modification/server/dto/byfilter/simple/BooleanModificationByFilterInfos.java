/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.simple;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.byfilter.DataType;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
public class BooleanModificationByFilterInfos extends AbstractSimpleModificationByFilterInfos<Boolean> {
    @Override
    @JsonProperty(access = JsonProperty.Access.READ_ONLY)
    public DataType getDataType() {
        return DataType.BOOLEAN;
    }
}
