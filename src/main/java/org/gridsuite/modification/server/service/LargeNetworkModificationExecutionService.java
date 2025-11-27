/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import com.powsybl.computation.ComputationManager;
import com.powsybl.computation.local.LocalComputationManager;
import io.micrometer.context.ContextExecutorService;
import io.micrometer.context.ContextSnapshotFactory;
import jakarta.annotation.PreDestroy;
import lombok.Getter;
import lombok.NonNull;
import lombok.SneakyThrows;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.function.Supplier;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
@Getter
public class LargeNetworkModificationExecutionService {

    private final ExecutorService executorService;

    private final ComputationManager computationManager;

    @SneakyThrows
    public LargeNetworkModificationExecutionService(@Value("${max-large-concurrent-applications}") int maxConcurrentLargeModifications,
                                                    @NonNull NetworkModificationObserver networkModificationObserver) {
        ThreadPoolExecutor threadPoolExecutor = (ThreadPoolExecutor) Executors.newFixedThreadPool(maxConcurrentLargeModifications);
        networkModificationObserver.createThreadPoolMetric(threadPoolExecutor);
        executorService = ContextExecutorService.wrap(threadPoolExecutor,
            () -> ContextSnapshotFactory.builder().build().captureAll());
        computationManager = new LocalComputationManager(getExecutorService());
    }

    @PreDestroy
    private void preDestroy() {
        executorService.shutdown();
    }

    public <U> CompletableFuture<U> supplyAsync(Supplier<U> supplier) {
        return CompletableFuture.supplyAsync(supplier, executorService);
    }
}
