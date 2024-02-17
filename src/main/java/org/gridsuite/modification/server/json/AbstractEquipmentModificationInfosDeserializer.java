/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonParser;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractEquipmentModificationInfosDeserializer<T extends EquipmentModificationInfos> extends AbstractModificationInfosDeserializer<T> {
    protected AbstractEquipmentModificationInfosDeserializer(final Class<T> t) {
        super(t);
    }

    protected void deserializeAttribute(EquipmentModificationInfos modificationInfos, JsonParser parser) throws IOException {
        switch (parser.getCurrentName()) {
            case "equipmentId":
                parser.nextToken();
                modificationInfos.setEquipmentId(parser.getValueAsString());
                break;

            default:
                super.deserializeAttribute(modificationInfos, parser);
        }
    }
}
