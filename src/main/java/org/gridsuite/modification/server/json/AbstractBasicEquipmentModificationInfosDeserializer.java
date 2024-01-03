/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonParser;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.BasicEquipmentModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractBasicEquipmentModificationInfosDeserializer<T extends BasicEquipmentModificationInfos> extends AbstractEquipmentModificationInfosDeserializer<T> {
    protected AbstractBasicEquipmentModificationInfosDeserializer(final Class<T> t) {
        super(t);
    }

    protected void deserializeAttribute(BasicEquipmentModificationInfos modificationInfos, JsonParser parser) throws IOException {
        switch (parser.getCurrentName()) {
            case "equipmentName":
                parser.nextToken();
                modificationInfos.setEquipmentName(parser.readValueAs(AttributeModification.class));
                break;

            default:
                super.deserializeAttribute(modificationInfos, parser);
        }
    }
}
