/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import org.gridsuite.modification.server.dto.ModificationInfos;

import java.io.IOException;
import java.time.ZonedDateTime;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractModificationInfosDeserializer<T extends ModificationInfos> extends StdDeserializer<T> {
    protected AbstractModificationInfosDeserializer(final Class<T> t) {
        super(t);
    }

    protected void deserializeAttribute(ModificationInfos modificationInfos, JsonParser parser) throws IOException {
        switch (parser.getCurrentName()) {
            case "version", "type":
                break;
            case "uuid":
                parser.nextToken();
                modificationInfos.setUuid(UUID.fromString(parser.getValueAsString()));
                break;
            case "date":
                parser.nextToken();
                modificationInfos.setDate(ZonedDateTime.parse(parser.getValueAsString()));
                break;
            case "stashed":
                parser.nextToken();
                modificationInfos.setStashed(parser.getValueAsBoolean());
                break;
            case "messageType":
                parser.nextToken();
                modificationInfos.setMessageType(parser.getValueAsString());
                break;
            case "messageValues":
                parser.nextToken();
                modificationInfos.setMessageValues(parser.getValueAsString());
                break;
            default:
                throw new IllegalStateException("Unexpected field: " + parser.getCurrentName());
        }
    }
}
