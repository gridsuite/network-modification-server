/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.modification.server.dto.LoadModificationInfos;

import java.io.IOException;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LoadModificationInfosDeserializer extends AbstractInjectionModificationInfosDeserializer<LoadModificationInfos> {
    LoadModificationInfosDeserializer() {
        super(LoadModificationInfos.class);
    }

    @Override
    public LoadModificationInfos deserialize(JsonParser parser, DeserializationContext ctx) throws IOException {
        LoadModificationInfos modificationInfos = new LoadModificationInfos();
        while (parser.nextToken() != JsonToken.END_OBJECT) {
            switch (parser.getCurrentName()) {
                case "loadType":
                    modificationInfos.setLoadType(attributeModificationInfosDeserializer.deserialize(parser, LoadType.class));
                    break;
                case "constantActivePower":
                    modificationInfos.setConstantActivePower(attributeModificationInfosDeserializer.deserialize(parser, Double.class));
                    break;
                case "constantReactivePower":
                    modificationInfos.setConstantReactivePower(attributeModificationInfosDeserializer.deserialize(parser, Double.class));
                    break;

                default:
                    super.deserializeAttribute(modificationInfos, parser);
            }
        }

        return modificationInfos;
    }
}
