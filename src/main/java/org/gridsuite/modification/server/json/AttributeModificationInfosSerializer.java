/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonGenerator;
import org.gridsuite.modification.server.dto.AttributeModification;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class AttributeModificationInfosSerializer {
    public void serialize(String fieldName, AttributeModification<?> modification, JsonGenerator jsonGenerator) throws IOException {
        if (modification == null) {
            return;
        }
        jsonGenerator.writeFieldName(fieldName);
        jsonGenerator.writeStartObject();
        jsonGenerator.writeObjectField("value", modification.getValue());
        jsonGenerator.writeStringField("op", modification.getOp().name());
        jsonGenerator.writeEndObject();
    }
}
