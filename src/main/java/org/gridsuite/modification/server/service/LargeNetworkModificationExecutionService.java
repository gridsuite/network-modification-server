/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import jakarta.annotation.PreDestroy;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.function.Supplier;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class LargeNetworkModificationExecutionService {

    private ThreadPoolExecutor executorService;

    public LargeNetworkModificationExecutionService(@Value("${max-large-concurrent-applications}") int maxConcurrentLargeModifications,
                                                    @NonNull NetworkModificationObserver networkModificationObserver) {
        executorService = (ThreadPoolExecutor) Executors.newFixedThreadPool(maxConcurrentLargeModifications);
        networkModificationObserver.createThreadPoolMetric(executorService);
    }

    @PreDestroy
    private void preDestroy() {
        executorService.shutdown();
    }

    public <U> CompletableFuture<U> supplyAsync(Supplier<U> supplier) {
        return CompletableFuture.supplyAsync(supplier, executorService);
    }
}
