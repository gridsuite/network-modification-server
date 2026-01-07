/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import org.springframework.stereotype.Component;

/**
 * @author Mohamed Ben-rejeb {@literal <mohamed.ben-rejeb at rte-france.com>}
 */
@Component
public class ServerNameProvider implements com.powsybl.ws.commons.error.ServerNameProvider {

    private final String name = "network-modification-server";

    public ServerNameProvider() {
    }

    @Override
    public String serverName() {
        return name;
    }
}
