/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.network.store.client.NetworkStoreService;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.dto.GroovyScriptResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.util.Set;
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

    public Set<String> changeSwitchState(UUID networkUuid, String switchId, String open) {
        Network network = getNetwork(networkUuid);

        Switch sw = network.getSwitch(switchId);
        if (sw == null) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Switch " + switchId + " not found");
        }

        boolean newOpen = Boolean.parseBoolean(open);
        if (sw.isOpen() != newOpen) {
            sw.setOpen(newOpen);

            networkStoreService.flush(network);

            return Set.of(sw.getVoltageLevel().getSubstation().getId());
        } else {
            return Set.of();
        }
    }

    public GroovyScriptResult applyGroovyScript(UUID networkUuid, String groovyScript) {
        CompilerConfiguration conf = new CompilerConfiguration();
        Network network = getNetwork(networkUuid);
        DefaultNetworkStoreListener listener = new DefaultNetworkStoreListener();
        network.addListener(listener);

        Binding binding = new Binding();
        binding.setProperty("network", network);
        GroovyShell shell = new GroovyShell(binding, conf);
        try {
            shell.evaluate(groovyScript);
            networkStoreService.flush(network);

            return new GroovyScriptResult(true, listener.getModifications());
        } catch (Exception ignored) {
            return new GroovyScriptResult(false, listener.getModifications());
        }
    }

}
