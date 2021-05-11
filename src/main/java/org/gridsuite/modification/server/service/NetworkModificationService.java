/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import java.util.List;
import java.util.UUID;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationService {

    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationService.class);

    private final NetworkStoreService networkStoreService;

    private final NetworkModificationRepository modificationRepository;

    public NetworkModificationService(NetworkStoreService networkStoreService, NetworkModificationRepository modificationRepository) {
        this.networkStoreService = networkStoreService;
        this.modificationRepository = modificationRepository;
    }

    public Flux<ElementaryModificationInfos> applyGroovyScript(UUID networkUuid, String groovyScript) {
        return assertGroovyScriptNotEmpty(groovyScript).thenMany(
                getNetwork(networkUuid).flatMapIterable(network -> doAction(network, networkUuid, () -> {
                    var conf = new CompilerConfiguration();
                    var binding = new Binding();
                    binding.setProperty("network", network);
                    var shell = new GroovyShell(binding, conf);
                    shell.evaluate(groovyScript);
                }, GROOVY_SCRIPT_ERROR))
        );
    }

    public Flux<ElementaryModificationInfos> changeSwitchState(UUID networkUuid, String switchId, boolean open) {
        return getNetwork(networkUuid)
                .filter(network -> network.getSwitch(switchId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(SWITCH_NOT_FOUND, switchId)))
                .filter(network -> network.getSwitch(switchId).isOpen() != open)
                .flatMapIterable(network -> doAction(network, networkUuid, () -> network.getSwitch(switchId).setOpen(open)));
    }

    public Flux<UUID> getModificationGroups() {
        return Flux.fromStream(() -> modificationRepository.getModificationGroupsUuids().stream());
    }

    public Flux<ModificationInfos> getModifications(UUID groupUuid) {
        return Flux.fromStream(() -> modificationRepository.getModifications(groupUuid).stream());
    }

    public Mono<ElementaryModificationInfos> getElementaryModification(UUID groupUuid, UUID modificationUuid) {
        return Mono.fromCallable(() -> modificationRepository.getElementaryModification(groupUuid, modificationUuid));
    }

    public Mono<Void> deleteModificationGroup(UUID groupUuid) {
        return Mono.fromRunnable(() -> modificationRepository.deleteModificationGroup(groupUuid));
    }

    private List<ElementaryModificationInfos> doAction(Network network, UUID networkUuid, Runnable modification) {
        return doAction(network, networkUuid, modification, MODIFICATION_ERROR);
    }

    private List<ElementaryModificationInfos> doAction(Network network, UUID networkUuid, Runnable action, NetworkModificationException.Type typeIfError) {
        try {
            var listener = NetworkStoreListener.create(network, networkUuid, modificationRepository);
            action.run();
            networkStoreService.flush(network);
            listener.saveModifications();
            return listener.getModifications();
        } catch (Exception e) {
            var exc = new NetworkModificationException(typeIfError, e);
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

    private Mono<Void> assertGroovyScriptNotEmpty(String groovyScript) {
        return StringUtils.isBlank(groovyScript) ? Mono.error(new NetworkModificationException(GROOVY_SCRIPT_EMPTY)) : Mono.empty();
    }
}
