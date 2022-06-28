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
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.BuildInfos;
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
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.logging.Level;

import static org.gridsuite.modification.server.service.BuildStoppedPublisherService.CANCEL_MESSAGE;
import static org.gridsuite.modification.server.service.BuildFailedPublisherService.FAIL_MESSAGE;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class BuildWorkerService {

    private static final Logger LOGGER = LoggerFactory.getLogger(BuildWorkerService.class);

    private static final String CATEGORY_BROKER_INPUT = BuildWorkerService.class.getName() + ".input-broker-messages";

    private static final String CATEGORY_BROKER_OUTPUT = BuildWorkerService.class.getName() + ".output-broker-messages";

    private static final Logger OUTPUT_MESSAGE_LOGGER = LoggerFactory.getLogger(CATEGORY_BROKER_OUTPUT);

    private NetworkModificationService networkModificationService;

    private ObjectMapper objectMapper;

    private BuildStoppedPublisherService stoppedPublisherService;

    private BuildFailedPublisherService failedPublisherService;

    private Map<String, CompletableFuture<List<ModificationInfos>>> futures = new ConcurrentHashMap<>();

    private Map<String, BuildCancelContext> cancelBuildRequests = new ConcurrentHashMap<>();

    private Set<String> buildRequests = Sets.newConcurrentHashSet();

    @Autowired
    private StreamBridge resultBuildMessagePublisher;

    public BuildWorkerService(@NonNull NetworkModificationService networkModificationService,
                              @NonNull ObjectMapper objectMapper,
                              @NonNull BuildStoppedPublisherService stoppedPublisherService,
                              @NonNull BuildFailedPublisherService failedPublisherService) {
        this.networkModificationService = networkModificationService;
        this.objectMapper = objectMapper;
        this.stoppedPublisherService = stoppedPublisherService;
        this.failedPublisherService = failedPublisherService;
    }

    private Mono<List<ModificationInfos>> execBuildVariant(Network network, BuildExecContext execContext, BuildInfos buildInfos) {
        UUID networkUuid = execContext.getNetworkUuid();
        String receiver = execContext.getReceiver();

        if (cancelBuildRequests.get(receiver) != null) {
            return Mono.empty();
        }

        CompletableFuture<List<ModificationInfos>> future = CompletableFuture.supplyAsync(() ->
            networkModificationService.applyModifications(network, networkUuid, buildInfos)
        );

        futures.put(receiver, future);

        if (cancelBuildRequests.get(receiver) != null) {
            return Mono.empty();
        } else {
            LOGGER.info("Starting build on variant : {}", buildInfos.getDestinationVariantId());
            return Mono.fromCompletionStage(future);
        }
    }

    @Bean
    public Consumer<Flux<Message<String>>> consumeBuild() {
        return f -> f.log(CATEGORY_BROKER_INPUT, Level.FINE)
            .flatMap(message -> {
                BuildExecContext execContext = BuildExecContext.fromMessage(message, objectMapper);
                if (StringUtils.isBlank(execContext.getReceiver())) {
                    return Mono.empty();
                }
                buildRequests.add(execContext.getReceiver()); // receiver is the node uuid to build

                BuildInfos buildInfos = execContext.getBuildInfos();

                return networkModificationService.cloneNetworkVariant(execContext.getNetworkUuid(),
                                                                      buildInfos.getOriginVariantId(),
                                                                      buildInfos.getDestinationVariantId())
                    .flatMap(network -> execBuildVariant(network, execContext, buildInfos))
                    .doOnSuccess(result -> {
                        if (result != null) {  // result available
                            Set<String> allSubstationsIds = new HashSet<>();
                            result.forEach(r -> allSubstationsIds.addAll(r.getSubstationIds()));

                            Message<String> sendMessage = MessageBuilder
                                .withPayload(String.join(",", allSubstationsIds))
                                .setHeader("receiver", execContext.getReceiver())
                                .build();

                            sendResultBuildMessage(sendMessage);
                            LOGGER.info("Build complete on node '{}'", execContext.getReceiver());
                        } else {  // result not available : stop build request
                            if (cancelBuildRequests.get(execContext.getReceiver()) != null) {
                                stoppedPublisherService.publishCancel(execContext.getReceiver());
                            }
                        }
                    })
                    .onErrorResume(t -> {
                        if (!(t instanceof CancellationException)) {
                            LOGGER.error(FAIL_MESSAGE, t);
                            failedPublisherService.publishFail(execContext.getReceiver(), t.getMessage());
                            return Mono.empty();
                        }
                        return Mono.empty();
                    })
                    .doFinally(s -> {
                        futures.remove(execContext.getReceiver());
                        cancelBuildRequests.remove(execContext.getReceiver());
                        buildRequests.remove(execContext.getReceiver());
                    });
            })
            .onErrorContinue((t, r) -> LOGGER.error("Exception in consumeBuild", t))
            .subscribe();
    }

    @Bean
    public Consumer<Flux<Message<String>>> consumeCancelBuild() {
        return f -> f.log(CATEGORY_BROKER_INPUT, Level.FINE)
            .flatMap(message -> {
                BuildCancelContext cancelContext = BuildCancelContext.fromMessage(message);

                if (buildRequests.contains(cancelContext.getReceiver())) {
                    cancelBuildRequests.put(cancelContext.getReceiver(), cancelContext);
                }

                // find the completableFuture associated with receiver
                CompletableFuture<List<ModificationInfos>> future = futures.get(cancelContext.getReceiver());
                if (future != null) {
                    future.cancel(true);  // cancel build in progress

                    return Mono.fromRunnable(() -> {
                        stoppedPublisherService.publishCancel(cancelContext.getReceiver());
                        LOGGER.info(CANCEL_MESSAGE + " (receiver='{}')", cancelContext.getReceiver());
                    });
                }
                return Mono.empty();
            })
            .onErrorContinue((t, r) -> LOGGER.error("Exception in consumeCancelBuild", t))
            .subscribe();
    }

    private void sendResultBuildMessage(Message<String> message) {
        OUTPUT_MESSAGE_LOGGER.debug("Sending message : {}", message);
        resultBuildMessagePublisher.send("publishResultBuild-out-0", message);
    }
}
