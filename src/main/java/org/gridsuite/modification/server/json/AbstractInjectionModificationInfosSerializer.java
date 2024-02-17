/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.gridsuite.modification.server.dto.InjectionModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractInjectionModificationInfosSerializer<T extends InjectionModificationInfos> extends AbstractBasicEquipmentModificationInfosSerializer<T> {
    protected AbstractInjectionModificationInfosSerializer(final Class<T> t) {
        super(t);
    }

    @Override
    public void serialize(T modification, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        super.serialize(modification, jsonGenerator, serializerProvider);
        attributeSerializer.serialize("voltageLevelId", modification.getVoltageLevelId(), jsonGenerator);
        attributeSerializer.serialize("busOrBusbarSectionId", modification.getBusOrBusbarSectionId(), jsonGenerator);
    }
}
