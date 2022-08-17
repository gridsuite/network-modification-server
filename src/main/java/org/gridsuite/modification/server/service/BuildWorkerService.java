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

import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;
import static org.gridsuite.modification.server.service.BuildStoppedPublisherService.CANCEL_MESSAGE;
import static org.gridsuite.modification.server.service.BuildFailedPublisherService.FAIL_MESSAGE;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class BuildWorkerService {

    private static final Logger LOGGER = LoggerFactory.getLogger(BuildWorkerService.class);
    private static final String CATEGORY_BROKER_OUTPUT = BuildWorkerService.class.getName() + ".output-broker-messages";

    private static final Logger OUTPUT_MESSAGE_LOGGER = LoggerFactory.getLogger(CATEGORY_BROKER_OUTPUT);

    private final NetworkModificationService networkModificationService;

    private final ObjectMapper objectMapper;

    private final BuildStoppedPublisherService stoppedPublisherService;

    private final BuildFailedPublisherService failedPublisherService;

    private final Map<String, CompletableFuture<List<ModificationInfos>>> futures = new ConcurrentHashMap<>();

    private final Map<String, BuildCancelContext> cancelBuildRequests = new ConcurrentHashMap<>();

    private final Set<String> buildRequests = Sets.newConcurrentHashSet();

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

    private List<ModificationInfos> execBuildVariant(Network network, BuildExecContext execContext, BuildInfos buildInfos) throws ExecutionException, InterruptedException {
        UUID networkUuid = execContext.getNetworkUuid();
        String receiver = execContext.getReceiver();
        if (cancelBuildRequests.get(receiver) != null) {
            return Collections.emptyList();
        }
        CompletableFuture<List<ModificationInfos>> future = CompletableFuture.supplyAsync(() ->
            networkModificationService.applyModifications(network, networkUuid, buildInfos)
        );
        futures.put(receiver, future);
        if (cancelBuildRequests.get(receiver) != null) {
            return Collections.emptyList();
        } else {
            LOGGER.info("Starting build on variant : {}", buildInfos.getDestinationVariantId());
        }
        return future.get();
    }

    @Bean
    public Consumer<Message<String>> consumeBuild() {
        return message -> {
            BuildExecContext execContext = BuildExecContext.fromMessage(message, objectMapper);
            try {
                buildRequests.add(execContext.getReceiver()); // receiver is the node uuid to build
                BuildInfos buildInfos = execContext.getBuildInfos();
                Network network = networkModificationService.cloneNetworkVariant(execContext.getNetworkUuid(),
                        buildInfos.getOriginVariantId(),
                        buildInfos.getDestinationVariantId());
                List<ModificationInfos> result = execBuildVariant(network, execContext, buildInfos);
                if (result != null) {  // result available
                    Set<String> allSubstationsIds = new HashSet<>();
                    result.forEach(r -> allSubstationsIds.addAll(r.getSubstationIds()));
                    Message<String> sendMessage = MessageBuilder
                                                          .withPayload(
                                                                  String.join(",", allSubstationsIds))
                                                          .setHeader(
                                                                  "receiver", execContext.getReceiver())
                                                          .build();
                    sendResultBuildMessage(sendMessage);
                    LOGGER.info("Build complete on node '{}'", execContext.getReceiver());
                } else {  // result not available : stop build request
                    if (cancelBuildRequests.get(execContext.getReceiver()) != null) {
                        stoppedPublisherService.publishCancel(execContext.getReceiver());
                    }
                }
            } catch (ExecutionException | InterruptedException e) {
                LOGGER.error("Exception in consumeBuild", e);
            } catch (Exception e) {
                if (!(e instanceof CancellationException)) {
                    LOGGER.error(FAIL_MESSAGE, e);
                    failedPublisherService.publishFail(execContext.getReceiver(), e.getMessage());
                }
            } finally {
                futures.remove(execContext.getReceiver());
                cancelBuildRequests.remove(execContext.getReceiver());
                buildRequests.remove(execContext.getReceiver());
                LOGGER.error("Exception in consumeBuild");
            }
        };
    }

    @Bean
    public Consumer<List<Message<String>>> consumeCancelBuild() {
        return message -> {
            BuildCancelContext cancelContext = BuildCancelContext.fromMessage((Message<String>) message);
            if (buildRequests.contains(cancelContext.getReceiver())) {
                cancelBuildRequests.put(cancelContext.getReceiver(), cancelContext);
            }
            // find the completableFuture associated with receiver
            CompletableFuture<List<ModificationInfos>> future = futures.get(cancelContext.getReceiver());
            if (future != null) {
                future.cancel(true);  // cancel build in progress
                stoppedPublisherService.publishCancel(cancelContext.getReceiver());
                LOGGER.info(CANCEL_MESSAGE + " (receiver='{}')", cancelContext.getReceiver());
            }
        };
    }

    private void sendResultBuildMessage(Message<String> message) {
        OUTPUT_MESSAGE_LOGGER.debug("Sending message : {}", message);
        resultBuildMessagePublisher.send("publishResultBuild-out-0", message);
    }
}
