/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.TabularModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class TabularModificationInfosSerializer extends AbstractModificationInfosSerializer<TabularModificationInfos> {

    TabularModificationInfosSerializer() {
        super(TabularModificationInfos.class);
    }

    @Override
    public void serialize(TabularModificationInfos modification, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        super.serialize(modification, jsonGenerator, serializerProvider);
        jsonGenerator.writeStringField("modificationType", modification.getModificationType());
        jsonGenerator.writeFieldName("modifications");
        jsonGenerator.writeStartArray();
        for (ModificationInfos m : modification.getModifications()) {
            jsonGenerator.writeObject(m);
        }
        jsonGenerator.writeEndArray();
    }
}
