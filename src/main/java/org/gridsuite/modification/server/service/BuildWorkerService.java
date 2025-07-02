/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Sets;
import lombok.NonNull;
import org.gridsuite.modification.server.BuildException;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class BuildWorkerService {

    private static final Logger LOGGER = LoggerFactory.getLogger(BuildWorkerService.class);

    public static final String CANCEL_MESSAGE = "Build was canceled";
    public static final String FAIL_MESSAGE = "Build has failed";

    private final NetworkModificationService networkModificationService;

    private final NetworkModificationObserver networkModificationObserver;

    private final ObjectMapper objectMapper;

    private final BuildStoppedPublisherService stoppedPublisherService;

    private final Map<String, CompletableFuture<NetworkModificationResult>> futures = new ConcurrentHashMap<>();

    private final Map<String, BuildCancelContext> cancelBuildRequests = new ConcurrentHashMap<>();

    private final Set<String> buildRequests = Sets.newConcurrentHashSet();

    private final Lock lockRunAndCancel = new ReentrantLock();

    @Autowired
    private NotificationService notificationService;

    public BuildWorkerService(@NonNull NetworkModificationService networkModificationService,
                              @NonNull NetworkModificationObserver networkModificationObserver,
                              @NonNull ObjectMapper objectMapper,
                              @NonNull BuildStoppedPublisherService stoppedPublisherService) {
        this.networkModificationService = networkModificationService;
        this.networkModificationObserver = networkModificationObserver;
        this.objectMapper = objectMapper;
        this.stoppedPublisherService = stoppedPublisherService;
    }

    private CompletableFuture<NetworkModificationResult> execBuildVariant(BuildExecContext execContext, BuildInfos buildInfos) {
        lockRunAndCancel.lock();
        try {
            UUID networkUuid = execContext.getNetworkUuid();
            String receiver = execContext.getReceiver();

            if (cancelBuildRequests.get(receiver) != null) {
                return null;
            }

            buildRequests.add(execContext.getReceiver()); // receiver is the node uuid to build

            CompletableFuture<NetworkModificationResult> future = CompletableFuture.supplyAsync(() -> {
                    LOGGER.info("Starting build on variant : {}", buildInfos.getDestinationVariantId());
                    return networkModificationService.buildVariant(networkUuid, buildInfos);
                }
            );

            futures.put(receiver, future);

            return future;
        } finally {
            lockRunAndCancel.unlock();
        }
    }

    @Bean
    public Consumer<Message<String>> consumeBuild() {
        return message -> networkModificationObserver.observeBuild(() -> {
            BuildExecContext execContext;
            try {
                execContext = BuildExecContext.fromMessage(message, objectMapper);
            } catch (Exception e) {
                throw new BuildException("Failed to read build message", e);
            }
            startBuild(Objects.requireNonNull(execContext));
        });
    }

    private void startBuild(BuildExecContext execContext) {
        try {
            BuildInfos buildInfos = execContext.getBuildInfos();
            CompletableFuture<NetworkModificationResult> future = execBuildVariant(execContext, buildInfos);
            NetworkModificationResult result;
            if (future != null && (result = future.join()) != null) {  // result available
                notificationService.emitBuildResultMessage(result, execContext.getReceiver(), execContext.getWorkflowType(), execContext.getWorkflowInfos());
                LOGGER.info("Build complete on node '{}'", execContext.getReceiver());
            } else {  // result not available : stop build request
                if (cancelBuildRequests.get(execContext.getReceiver()) != null) {
                    stoppedPublisherService.publishCancel(execContext.getReceiver(), CANCEL_MESSAGE, execContext.getWorkflowType(), execContext.getWorkflowInfos());
                }
            }
        } catch (CancellationException e) {
            stoppedPublisherService.publishCancel(execContext.getReceiver(), CANCEL_MESSAGE, execContext.getWorkflowType(), execContext.getWorkflowInfos());
        } catch (Exception e) {
            throw new BuildException("Node build failed", e);
        } finally {
            futures.remove(execContext.getReceiver());
            cancelBuildRequests.remove(execContext.getReceiver());
            buildRequests.remove(execContext.getReceiver());
        }
    }

    @Bean
    public Consumer<Message<String>> consumeCancelBuild() {
        return message -> {
            BuildCancelContext execContext = null;
            try {
                execContext = BuildCancelContext.fromMessage(message);
            } catch (Exception e) {
                LOGGER.error("Error retrieving message in consumeCancelBuild", e);
            }
            cancelBuild(Objects.requireNonNull(execContext));
        };
    }

    private void cancelBuild(BuildCancelContext cancelContext) {
        lockRunAndCancel.lock();
        try {
            cancelBuildRequests.put(cancelContext.getReceiver(), cancelContext);

            // find the completableFuture associated with receiver
            CompletableFuture<NetworkModificationResult> future = futures.get(cancelContext.getReceiver());
            if (future != null) {
                future.cancel(true);  // cancel build in progress
                LOGGER.info(CANCEL_MESSAGE + " (receiver='{}')", cancelContext.getReceiver());
            }
        } catch (Exception e) {
            LOGGER.error("Cancel has failed", e);
        } finally {
            lockRunAndCancel.unlock();
        }
    }
}
