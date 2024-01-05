/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonParser;
import org.gridsuite.modification.server.dto.InjectionModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractInjectionModificationInfosDeserializer<T extends InjectionModificationInfos> extends AbstractBasicEquipmentModificationInfosDeserializer<T> {
    protected AbstractInjectionModificationInfosDeserializer(final Class<T> t) {
        super(t);
    }

    protected void deserializeAttribute(InjectionModificationInfos modificationInfos, JsonParser parser) throws IOException {
        switch (parser.getCurrentName()) {
            case "voltageLevelId":
                modificationInfos.setVoltageLevelId(attributeModificationInfosDeserializer.deserialize(parser, String.class));
                break;

            case "busOrBusbarSectionId":
                modificationInfos.setBusOrBusbarSectionId(attributeModificationInfosDeserializer.deserialize(parser, String.class));
                break;

            default:
                super.deserializeAttribute(modificationInfos, parser);
        }
    }
}
