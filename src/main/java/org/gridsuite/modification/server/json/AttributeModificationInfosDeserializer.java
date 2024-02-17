/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class AttributeModificationInfosDeserializer {

    public <T> AttributeModification<T> deserialize(JsonParser parser, Class<T> type) throws IOException {
        if (parser.nextToken() == JsonToken.VALUE_NULL) {
            return null;
        }
        T value = null;
        OperationType op = null;
        while (parser.nextToken() != JsonToken.END_OBJECT) {
            switch (parser.getCurrentName()) {
                case "op":
                    parser.nextToken();
                    op = parser.readValueAs(OperationType.class);
                    break;

                case "value":
                    parser.nextToken();
                    value = parser.readValueAs(type);
                    break;

                default:
                    throw new IllegalStateException("Unexpected field: " + parser.getCurrentName());
            }
        }

        return AttributeModification.toAttributeModification(value, op);
    }
}

