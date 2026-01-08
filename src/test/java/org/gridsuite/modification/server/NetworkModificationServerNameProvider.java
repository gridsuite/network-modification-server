/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server;

import com.powsybl.ws.commons.error.ServerNameProvider;
import org.springframework.stereotype.Component;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Component
public class NetworkModificationServerNameProvider implements ServerNameProvider {

    @Override
    public String serverName() {
        return "network-modification-server";
    }
}
