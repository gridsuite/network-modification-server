/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.gridsuite.modification.server.dto.LoadModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LoadModificationInfosSerializer extends AbstractModificationInfosSerializer<LoadModificationInfos> {

    private final transient AttributeModificationInfosSerializer attributeSerializer = new AttributeModificationInfosSerializer();

    LoadModificationInfosSerializer() {
        super(LoadModificationInfos.class);
    }

    @Override
    public void serialize(LoadModificationInfos modification, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        super.serialize(modification, jsonGenerator, serializerProvider);
        jsonGenerator.writeStringField("equipmentId", modification.getEquipmentId());
        attributeSerializer.serialize("loadType", modification.getLoadType(), jsonGenerator);
        attributeSerializer.serialize("constantActivePower", modification.getConstantActivePower(), jsonGenerator);
        attributeSerializer.serialize("constantReactivePower", modification.getConstantReactivePower(), jsonGenerator);
    }
}
