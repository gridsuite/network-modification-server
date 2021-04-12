/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import com.powsybl.sld.iidm.extensions.BranchStatusAdder;
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

import java.util.List;
import java.util.UUID;

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

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected();
        if (!terminal1Disconnected) {
            terminal1Disconnected = terminal1.disconnect();
        }
        Terminal terminal2 = network.getLine(lineId).getTerminal1();
        boolean terminal2Disconnected = !terminal2.isConnected();
        if (!terminal2Disconnected) {
            terminal2Disconnected = terminal2.disconnect();
        }
        return terminal1Disconnected && terminal2Disconnected;
    }

    public Flux<ElementaryModificationInfos> lockoutLine(UUID networkUuid, String lineId) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doModification(network, networkUuid, () -> {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends");
                    }
                }
                ));
    }

    public Flux<ElementaryModificationInfos> tripLine(UUID networkUuid, String lineId) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doModification(network, networkUuid, () -> {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends");
                    }
                }
                ));
    }

    public Flux<ElementaryModificationInfos> energiseLineEnd(UUID networkUuid, String lineId, String side) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doModification(network, networkUuid, () -> {
                    Terminal terminalToConnect = network.getLine(lineId).getTerminal(Branch.Side.valueOf(side));
                    boolean isTerminalToConnectConnected = terminalToConnect.isConnected();
                    if (!isTerminalToConnectConnected) {
                        isTerminalToConnectConnected = terminalToConnect.connect();
                    }
                    Terminal terminalToDisconnect = network.getLine(lineId).getTerminal(Branch.Side.valueOf(side) == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE);
                    boolean isTerminalToDisconnectDisconnected = !terminalToDisconnect.isConnected();
                    if (!isTerminalToDisconnectDisconnected) {
                        isTerminalToDisconnectDisconnected = terminalToDisconnect.disconnect();
                    }
                    if (isTerminalToConnectConnected && isTerminalToDisconnectDisconnected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to energise line end");
                    }

                }
                ));
    }

    public Flux<ElementaryModificationInfos> switchOnLine(UUID networkUuid, String lineId) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doModification(network, networkUuid, () -> {
                    Terminal terminal1 = network.getLine(lineId).getTerminal1();
                    boolean terminal1Connected = terminal1.isConnected();
                    if (!terminal1Connected) {
                        terminal1Connected = terminal1.connect();
                    }
                    Terminal terminal2 = network.getLine(lineId).getTerminal2();
                    boolean terminal2Connected = terminal2.isConnected();
                    if (!terminal2Connected) {
                        terminal2Connected = terminal2.connect();
                    }
                    if (terminal1Connected && terminal2Connected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to connect both line ends");
                    }
                }
                ));
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
            saveModifications(listener);
            return listener.getModifications();
        } catch (Exception e) {
            var exc = new NetworkModificationException(typeIfError, e);
            LOGGER.error(exc.getMessage());
            throw exc;
        }
    }

    private void saveModifications(NetworkStoreListener listener) {
        listener.saveModifications();
        try {
            networkStoreService.flush(listener.getNetwork());
        } catch (Exception e) {
            listener.deleteModifications();
            throw e;
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
