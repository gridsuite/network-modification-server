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
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.TabularModificationInfos;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class TabularModificationInfosDeserializer extends AbstractModificationInfosDeserializer<TabularModificationInfos> {
    TabularModificationInfosDeserializer() {
        super(TabularModificationInfos.class);
    }

    @Override
    public TabularModificationInfos deserialize(JsonParser parser, DeserializationContext ctx) throws IOException {
        TabularModificationInfos modificationInfos = new TabularModificationInfos();
        while (parser.nextToken() != JsonToken.END_OBJECT) {
            switch (parser.getCurrentName()) {
                case "modificationType":
                    parser.nextToken();
                    modificationInfos.setModificationType(parser.getValueAsString());
                    break;

                case "modifications":
                    parser.nextToken();
                    modificationInfos.setModifications(deserializeModifications(parser, ctx));
                    break;

                default:
                    super.deserializeAttribute(modificationInfos, parser);
            }
        }

        return modificationInfos;
    }

    public List<ModificationInfos> deserializeModifications(JsonParser parser, DeserializationContext ctx) throws IOException {
        List<ModificationInfos> modifications = new ArrayList<>();
        while (parser.nextToken() != JsonToken.END_ARRAY) {
            modifications.add(new LoadModificationInfosDeserializer().deserialize(parser, ctx));
        }
        return modifications;
    }
}
