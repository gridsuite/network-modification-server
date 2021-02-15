/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@ComponentScan(basePackageClasses = {NetworkStoreService.class})
@Service
class NetworkModificationService {

    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationService.class);

    @Autowired
    private NetworkStoreService networkStoreService;

    private Set<String> doModification(Network network, Runnable modification) {
        return doModification(network, modification, MODIFICATION_ERROR);
    }

    private Set<String> doModification(Network network, Runnable modification, NetworkModificationException.Type typeIfError) {
        try {
            DefaultNetworkStoreListener listener = new DefaultNetworkStoreListener();
            network.addListener(listener);
            modification.run();

            networkStoreService.flush(network);

            return listener.getModifications();
        } catch (Exception e) {
            NetworkModificationException exc = new NetworkModificationException(typeIfError, e);
            LOGGER.error(exc.getMessage());
            throw exc;
        }
    }

    private Mono<Network> getNetwork(UUID networkUuid) {
        return Mono.fromCallable(() -> {
            try {
                return networkStoreService.getNetwork(networkUuid);
            } catch (PowsyblException e) {
                throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
            }
        }).subscribeOn(Schedulers.boundedElastic());
    }

    Mono<Set<String>> changeSwitchState(UUID networkUuid, String switchId, boolean open) {
        return getNetwork(networkUuid)
                .filter(network -> network.getSwitch(switchId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(SWITCH_NOT_FOUND, switchId)))
                .filter(network -> network.getSwitch(switchId).isOpen() != open)
                .map(network -> doModification(network, () -> network.getSwitch(switchId).setOpen(open)))
                .switchIfEmpty(Mono.just(Set.of()));
    }

    private Mono<Void> assertGroovyScriptNotEmpty(String groovyScript) {
        return StringUtils.isBlank(groovyScript) ? Mono.error(new NetworkModificationException(GROOVY_SCRIPT_EMPTY)) : Mono.empty();
    }

    Mono<Set<String>> applyGroovyScript(UUID networkUuid, String groovyScript) {
        return assertGroovyScriptNotEmpty(groovyScript).then(
                getNetwork(networkUuid).map(network -> doModification(network, () -> {
                    CompilerConfiguration conf = new CompilerConfiguration();
                    Binding binding = new Binding();
                    binding.setProperty("network", network);
                    GroovyShell shell = new GroovyShell(binding, conf);
                    shell.evaluate(groovyScript);
                }, GROOVY_SCRIPT_ERROR))
        );
    }
}
