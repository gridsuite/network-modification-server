/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Sets;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.RealizationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.context.annotation.Bean;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.logging.Level;

import static org.gridsuite.modification.server.service.RealizationStoppedPublisherService.CANCEL_MESSAGE;
import static org.gridsuite.modification.server.service.RealizationStoppedPublisherService.FAIL_MESSAGE;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class RealizationWorkerService {

    private static final Logger LOGGER = LoggerFactory.getLogger(RealizationWorkerService.class);

    private static final String CATEGORY_BROKER_INPUT = RealizationWorkerService.class.getName() + ".input-broker-messages";

    private static final String CATEGORY_BROKER_OUTPUT = RealizationWorkerService.class.getName() + ".output-broker-messages";

    private static final Logger OUTPUT_MESSAGE_LOGGER = LoggerFactory.getLogger(CATEGORY_BROKER_OUTPUT);

    private NetworkModificationService networkModificationService;

    private ObjectMapper objectMapper;

    private RealizationStoppedPublisherService stoppedPublisherService;

    private Map<String, CompletableFuture<List<EquipmenModificationInfos>>> futures = new ConcurrentHashMap<>();

    private Map<String, RealizationCancelContext> cancelRealizationRequests = new ConcurrentHashMap<>();

    private Set<String> realizationRequests = Sets.newConcurrentHashSet();

    @Autowired
    private StreamBridge resultRealizationMessagePublisher;

    public RealizationWorkerService(NetworkModificationService networkModificationService,
                                    ObjectMapper objectMapper,
                                    RealizationStoppedPublisherService stoppedPublisherService) {
        this.networkModificationService = Objects.requireNonNull(networkModificationService);
        this.objectMapper = Objects.requireNonNull(objectMapper);
        this.stoppedPublisherService = Objects.requireNonNull(stoppedPublisherService);
    }

    private Mono<List<EquipmenModificationInfos>> execRealizeVariant(Network network, UUID networkUuid, String receiver, RealizationInfos realizationInfos) {
        if (receiver != null && cancelRealizationRequests.get(receiver) != null) {
            return Mono.empty();
        }

        CompletableFuture<List<EquipmenModificationInfos>> future = CompletableFuture.supplyAsync(() ->
            networkModificationService.applyModifications(network, networkUuid, realizationInfos)
        );

        if (receiver != null) {
            futures.put(receiver, future);
        }
        if (receiver != null && cancelRealizationRequests.get(receiver) != null) {
            return Mono.empty();
        } else {
            LOGGER.info("Starting realization on variant : {}", realizationInfos.getDestinationVariantId());
            try {
                return Mono.fromCompletionStage(future);
            } catch (Exception exc) {
                return Mono.empty();
            }
        }
    }

    @Bean
    public Consumer<Flux<Message<String>>> consumeRealize() {
        return f -> f.log(CATEGORY_BROKER_INPUT, Level.FINE)
            .flatMap(message -> {
                RealizationExecContext execContext = RealizationExecContext.fromMessage(message, objectMapper);
                realizationRequests.add(execContext.getReceiver()); // receiver is the node uuid to realize

                RealizationInfos realizationInfos = execContext.getRealizationInfos();

                return networkModificationService.cloneNetworkVariant(execContext.getNetworkUuid(),
                                                                      realizationInfos.getOriginVariantId(),
                                                                      realizationInfos.getDestinationVariantId())
                    .flatMap(network -> execRealizeVariant(network, execContext.getNetworkUuid(), execContext.getReceiver(), realizationInfos))
                    .doOnSuccess(result -> {
                        if (result != null) {  // result available
                            Set<String> allSubstationsIds = new HashSet<>();
                            result.forEach(r -> allSubstationsIds.addAll(r.getSubstationIds()));

                            Message<String> sendMessage = MessageBuilder
                                .withPayload(String.join(",", allSubstationsIds))
                                .setHeader("receiver", execContext.getReceiver())
                                .build();

                            sendResultRealizationMessage(sendMessage);
                            LOGGER.info("Realization complete on node '{}'", execContext.getReceiver());
                        } else {  // result not available : stop realization request
                            if (cancelRealizationRequests.get(execContext.getReceiver()) != null) {
                                stoppedPublisherService.publishCancel(execContext.getReceiver());
                            }
                        }
                    })
                    .onErrorResume(throwable -> {
                        if (!(throwable instanceof CancellationException)) {
                            LOGGER.error(FAIL_MESSAGE, throwable);
                            stoppedPublisherService.publishFail(execContext.getReceiver(), throwable.getMessage());
                        }
                        return Mono.empty();
                    })
                    .doFinally(s -> {
                        futures.remove(execContext.getReceiver());
                        cancelRealizationRequests.remove(execContext.getReceiver());
                        realizationRequests.remove(execContext.getReceiver());
                    });
            })
            .subscribe();
    }

    @Bean
    public Consumer<Flux<Message<String>>> consumeCancelRealization() {
        return f -> f.log(CATEGORY_BROKER_INPUT, Level.FINE)
            .flatMap(message -> {
                RealizationCancelContext cancelContext = RealizationCancelContext.fromMessage(message);

                if (realizationRequests.contains(cancelContext.getReceiver())) {
                    cancelRealizationRequests.put(cancelContext.getReceiver(), cancelContext);
                }

                // find the completableFuture associated with receiver
                CompletableFuture<List<EquipmenModificationInfos>> future = futures.get(cancelContext.getReceiver());
                if (future != null) {
                    future.cancel(true);  // cancel realization in progress

                    return Mono.fromRunnable(() -> {
                        stoppedPublisherService.publishCancel(cancelContext.getReceiver());
                        LOGGER.info(CANCEL_MESSAGE + " (receiver='{}')", cancelContext.getReceiver());
                    });
                }
                return Mono.empty();
            })
            .onErrorContinue((t, r) -> LOGGER.error("Exception in consumeCancelRealization", t))
            .subscribe();
    }

    private void sendResultRealizationMessage(Message<String> message) {
        OUTPUT_MESSAGE_LOGGER.debug("Sending message : {}", message);
        resultRealizationMessagePublisher.send("publishResultRealization-out-0", message);
    }
}
