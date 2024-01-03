/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.json;

import org.gridsuite.modification.server.dto.ModificationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ModificationInfosSerializer extends AbstractModificationInfosSerializer<ModificationInfos> {
    ModificationInfosSerializer() {
        super(ModificationInfos.class);
    }
}
