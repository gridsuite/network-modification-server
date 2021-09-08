/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import com.powsybl.sld.iidm.extensions.BranchStatusAdder;
import groovy.lang.Binding;
import groovy.lang.GroovyShell;
import org.apache.commons.lang3.StringUtils;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ElementaryAttributeModificationInfos;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
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

    private final NetworkStoreService networkStoreService;

    private final NetworkModificationRepository modificationRepository;

    public NetworkModificationService(NetworkStoreService networkStoreService, NetworkModificationRepository modificationRepository) {
        this.networkStoreService = networkStoreService;
        this.modificationRepository = modificationRepository;
    }

    public Flux<ElementaryAttributeModificationInfos> applyGroovyScript(UUID networkUuid, UUID groupUuid, String groovyScript) {
        return assertGroovyScriptNotEmpty(groovyScript).thenMany(
                getNetwork(networkUuid).flatMapIterable(network -> doAction(network, groupUuid, () -> {
                    var conf = new CompilerConfiguration();
                    var binding = new Binding();
                    binding.setProperty("network", network);
                    var shell = new GroovyShell(binding, conf);
                    shell.evaluate(groovyScript);
                }, GROOVY_SCRIPT_ERROR))
        );
    }

    public Flux<ElementaryAttributeModificationInfos> changeSwitchState(UUID networkUuid, UUID groupUuid, String switchId, boolean open) {
        return getNetwork(networkUuid)
                .filter(network -> network.getSwitch(switchId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(SWITCH_NOT_FOUND, switchId)))
                .filter(network -> network.getSwitch(switchId).isOpen() != open)
                .flatMapIterable(network -> doAction(network, groupUuid, () -> network.getSwitch(switchId).setOpen(open)));
    }

    public Flux<UUID> getModificationGroups() {
        return Flux.fromStream(() -> modificationRepository.getModificationGroupsUuids().stream());
    }

    public Flux<ModificationInfos> getModifications(UUID groupUuid) {
        return Flux.fromStream(() -> modificationRepository.getModifications(groupUuid).stream());
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();
        return terminal1Disconnected && terminal2Disconnected;
    }

    public Flux<ElementaryAttributeModificationInfos> changeLineStatus(UUID networkUuid, UUID groupUuid, String lineId, String lineStatus) {
        Flux<ElementaryAttributeModificationInfos> modifications;
        switch (lineStatus) {
            case "lockout":
                modifications = lockoutLine(networkUuid, groupUuid, lineId);
                break;
            case "trip":
                modifications = tripLine(networkUuid, groupUuid, lineId);
                break;
            case "switchOn":
                modifications = switchOnLine(networkUuid, groupUuid, lineId);
                break;
            case "energiseEndOne":
                modifications = energiseLineEnd(networkUuid, groupUuid, lineId, Branch.Side.ONE);
                break;
            case "energiseEndTwo":
                modifications = energiseLineEnd(networkUuid, groupUuid, lineId, Branch.Side.TWO);
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + lineStatus);
        }
        return modifications;
    }

    public Flux<ElementaryAttributeModificationInfos> lockoutLine(UUID networkUuid, UUID groupUuid, String lineId) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doAction(network, groupUuid, () -> {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends");
                    }
                }
                ));
    }

    public Flux<ElementaryAttributeModificationInfos> tripLine(UUID networkUuid, UUID groupUuid, String lineId) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doAction(network, groupUuid, () -> {
                    if (disconnectLineBothSides(network, lineId)) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends");
                    }
                }
                ));
    }

    public Flux<ElementaryAttributeModificationInfos> energiseLineEnd(UUID networkUuid, UUID groupUuid, String lineId, Branch.Side side) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doAction(network, groupUuid, () -> {
                    Terminal terminalToConnect = network.getLine(lineId).getTerminal(side);
                    boolean isTerminalToConnectConnected = terminalToConnect.isConnected() || terminalToConnect.connect();
                    Terminal terminalToDisconnect = network.getLine(lineId).getTerminal(side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE);
                    boolean isTerminalToDisconnectDisconnected = !terminalToDisconnect.isConnected() || terminalToDisconnect.disconnect();
                    if (isTerminalToConnectConnected && isTerminalToDisconnectDisconnected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to energise line end");
                    }

                }
                ));
    }

    public Flux<ElementaryAttributeModificationInfos> switchOnLine(UUID networkUuid, UUID groupUuid, String lineId) {
        return getNetwork(networkUuid)
                .filter(network -> network.getLine(lineId) != null)
                .switchIfEmpty(Mono.error(new NetworkModificationException(LINE_NOT_FOUND, lineId)))
                .flatMapIterable(network -> doAction(network, groupUuid, () -> {
                    Terminal terminal1 = network.getLine(lineId).getTerminal1();
                    boolean terminal1Connected = terminal1.isConnected() || terminal1.connect();
                    Terminal terminal2 = network.getLine(lineId).getTerminal2();
                    boolean terminal2Connected = terminal2.isConnected() || terminal2.connect();
                    if (terminal1Connected && terminal2Connected) {
                        network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
                    } else {
                        throw new NetworkModificationException(MODIFICATION_ERROR, "Unable to connect both line ends");
                    }
                }
                ));
    }

    public Mono<ElementaryAttributeModificationInfos> getElementaryModification(UUID groupUuid, UUID modificationUuid) {
        return Mono.fromCallable(() -> modificationRepository.getElementaryModification(groupUuid, modificationUuid));
    }

    public Mono<Void> deleteModificationGroup(UUID groupUuid) {
        return Mono.fromRunnable(() -> modificationRepository.deleteModificationGroup(groupUuid));
    }

    private List<ElementaryAttributeModificationInfos> doAction(Network network, UUID groupUuid, Runnable modification) {
        return doAction(network, groupUuid, modification, MODIFICATION_ERROR);
    }

    private List<ElementaryAttributeModificationInfos> doAction(Network network, UUID groupUuid, Runnable action, NetworkModificationException.Type typeIfError) {
        try {
            var listener = NetworkStoreListener.create(network, groupUuid, modificationRepository);
            action.run();
            saveModifications(listener);
            return listener.getModifications();
        } catch (NetworkModificationException e) {
            throw e;
        } catch (Exception e) {
            throw new NetworkModificationException(typeIfError, e);
        }
    }

    private List<ElementaryModificationInfos> doActionCreation(NetworkStoreListener listener, Runnable action, NetworkModificationException.Type typeIfError) {
        try {
            action.run();
            saveCreations(listener);
            return listener.getCreations();
        } catch (NetworkModificationException e) {
            throw e;
        } catch (Exception e) {
            throw new NetworkModificationException(typeIfError, e);
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

    private void saveCreations(NetworkStoreListener listener) {
        listener.saveCreations();
        try {
            networkStoreService.flush(listener.getNetwork());
        } catch (Exception e) {
            listener.deleteCreations();
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

    private void createLoadInNodeBreaker(VoltageLevel voltageLevel, LoadCreationInfos loadCreationInfos) {
        // busId is a busbar section id
        VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
        BusbarSection busbarSection = nodeBreakerView.getBusbarSection(loadCreationInfos.getBusId());
        if (busbarSection == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, loadCreationInfos.getBusId());
        }

        // creating the disconnector
        int newNode = nodeBreakerView.getMaximumNodeIndex();
        nodeBreakerView.newSwitch()
                .setId("disconnector_" + loadCreationInfos.getEquipmentId())
                .setName("disconnector_" + loadCreationInfos.getEquipmentName())
                .setKind(SwitchKind.DISCONNECTOR)
                .setRetained(false)
                .setOpen(false)
                .setFictitious(false)
                .setNode1(busbarSection.getTerminal().getNodeBreakerView().getNode())
                .setNode2(newNode + 1)
                .add();

        // creating the breaker
        nodeBreakerView.newSwitch()
            .setId("breaker_" + loadCreationInfos.getEquipmentId())
            .setName("breaker_" + loadCreationInfos.getEquipmentName())
            .setKind(SwitchKind.BREAKER)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(newNode + 1)
            .setNode2(newNode + 2)
            .add();

        // creating the load
        voltageLevel.newLoad()
                .setId(loadCreationInfos.getEquipmentId())
                .setName(loadCreationInfos.getEquipmentName())
                .setNode(newNode + 2)
                .setP0(loadCreationInfos.getActivePower())
                .setQ0(loadCreationInfos.getReactivePower())
                .add();
    }

    private void createLoadInBusBreaker(VoltageLevel voltageLevel, LoadCreationInfos loadCreationInfos) {
        // busId is a bus id
        VoltageLevel.BusBreakerView busBreakerView = voltageLevel.getBusBreakerView();
        Bus bus = busBreakerView.getBus(loadCreationInfos.getBusId());
        if (bus == null) {
            throw new NetworkModificationException(BUS_NOT_FOUND, loadCreationInfos.getBusId());
        }

        // creating the load
        voltageLevel.newLoad()
            .setId(loadCreationInfos.getEquipmentId())
            .setName(loadCreationInfos.getEquipmentName())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setP0(loadCreationInfos.getActivePower())
            .setQ0(loadCreationInfos.getReactivePower())
            .add();
    }

    public Flux<ElementaryModificationInfos> createLoad(UUID networkUuid, UUID groupUuid, LoadCreationInfos loadCreationInfos) {
        return assertLoadCreationInfosNotEmpty(loadCreationInfos).thenMany(
                getNetwork(networkUuid).flatMapIterable(network -> {
                    NetworkStoreListener listener = NetworkStoreListener.create(network, groupUuid, modificationRepository);
                    return doActionCreation(listener, () -> {
                        // create the load in the network
                        VoltageLevel voltageLevel = network.getVoltageLevel(loadCreationInfos.getVoltageLevelId());
                        if (voltageLevel == null) {
                            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, loadCreationInfos.getVoltageLevelId());
                        }
                        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
                            createLoadInNodeBreaker(voltageLevel, loadCreationInfos);
                        } else {
                            createLoadInBusBreaker(voltageLevel, loadCreationInfos);
                        }

                        // add the load creation entity to the listener
                        listener.storeLoadCreation(loadCreationInfos);
                    }, CREATE_LOAD_ERROR);
                }));
    }

    private Mono<Void> assertLoadCreationInfosNotEmpty(LoadCreationInfos loadCreationInfos) {
        return loadCreationInfos == null ? Mono.error(new NetworkModificationException(CREATE_LOAD_ERROR, "Missing required attributes to create the load")) : Mono.empty();
    }
}
