/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.json;

import com.fasterxml.jackson.databind.module.SimpleModule;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.TabularModificationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ModificationInfosJsonModule extends SimpleModule {
    public ModificationInfosJsonModule() {
        //this.addSerializer(ModificationInfos.class, new ModificationInfosSerializer());
        this.addSerializer(LoadModificationInfos.class, new LoadModificationInfosSerializer());
        this.addSerializer(TabularModificationInfos.class, new TabularModificationInfosSerializer());
        this.addDeserializer(LoadModificationInfos.class, new LoadModificationInfosDeserializer());
        this.addDeserializer(TabularModificationInfos.class, new TabularModificationInfosDeserializer());
    }
}
