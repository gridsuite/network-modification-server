/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.network.store.client.NetworkStoreService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@ComponentScan(basePackageClasses = {NetworkStoreService.class})
@Service
class NetworkModificationService {

    @Autowired
    private NetworkStoreService networkStoreService;

    private Network getNetwork(UUID networkUuid) {
        try {
            return networkStoreService.getNetwork(networkUuid);
        } catch (PowsyblException e) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Network '" + networkUuid + "' not found");
        }
    }

    void changeSwitchState(UUID networkUuid, String switchId, String open) {
        Network network = getNetwork(networkUuid);
        Switch sw = network.getSwitch(switchId);
        if (sw == null) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Switch " + switchId + " not found");
        }

        sw.setOpen(Boolean.parseBoolean(open));

        networkStoreService.flush(network);
    }

    boolean lockoutLine(UUID networkUuid, String lineId, boolean lockout) {
        Network network = networkStoreService.getNetwork(networkUuid);
        Line line = network.getLine(lineId);
        if (line == null) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Line " + lineId + " not found");
        }

        boolean b1;
        boolean b2;
        if (lockout) {
            b1 = line.getTerminal1().disconnect();
            b2 = line.getTerminal2().disconnect();
        } else {
            b1 = line.getTerminal1().connect();
            b2 = line.getTerminal2().connect();
        }
        networkStoreService.flush(network);
        return b1 || b2;
    }
}
