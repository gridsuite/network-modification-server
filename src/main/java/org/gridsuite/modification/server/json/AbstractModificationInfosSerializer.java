/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.jsontype.TypeSerializer;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import org.gridsuite.modification.server.dto.ModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractModificationInfosSerializer<T extends ModificationInfos> extends StdSerializer<T> {

    protected AbstractModificationInfosSerializer(final Class<T> t) {
        super(t);
    }

    @Override
    public void serialize(final T modification, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        jsonGenerator.writeStringField("version", ModificationInfos.VERSION);
        jsonGenerator.writeStringField("uuid", modification.getUuid().toString());
        jsonGenerator.writeStringField("type", modification.getType().name());
        jsonGenerator.writeStringField("date", modification.getDate().toString());
        jsonGenerator.writeBooleanField("stashed", modification.getStashed());
        if (modification.getMessageType() != null) {
            jsonGenerator.writeStringField("messageType", modification.getMessageType());
        }
        if (modification.getMessageValues() != null) {
            jsonGenerator.writeStringField("messageValues", modification.getMessageValues());
        }
    }

    @Override
    public void serializeWithType(T modification, JsonGenerator gen,
                                  SerializerProvider provider, TypeSerializer typeSer) throws IOException {
        typeSer.writeTypePrefixForObject(modification, gen);
        serialize(modification, gen, provider);
        typeSer.writeTypeSuffixForObject(modification, gen);
    }
}
