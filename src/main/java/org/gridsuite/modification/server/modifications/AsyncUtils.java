/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.gridsuite.modification.server.dto.NetworkModificationResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class AsyncUtils {
    private AsyncUtils() {
        // Should not be instantiated
    }

    // The signature of this method is chosen so that we can implement easily sequential or parallel schedule
    // If we change it (for example to parallel scheduling), we should keep the exceptional behavior consistent,
    // call the apply function inside a thenCompose anyway to wrap its exceptions in exceptional future completions.
    public static CompletableFuture<List<Optional<NetworkModificationResult>>> scheduleApplyModifications(
        Function<ModificationApplicationContext, CompletableFuture<Optional<NetworkModificationResult>>> func,
        List<ModificationApplicationContext> applicationContexts) {
        List<CompletableFuture<Optional<NetworkModificationResult>>> results = new ArrayList<>(applicationContexts.size());
        CompletableFuture<?> chainedFutures = CompletableFuture.completedFuture(null);
        for (ModificationApplicationContext applicationContext : applicationContexts) {
            chainedFutures = chainedFutures.thenCompose(unused -> {
                var cf = func.apply(applicationContext);
                // thencompose, this should add the computation result to the list and
                // and schedule the next computation in the same thread as the task
                // The list is accessed from different threads but not concurrently and
                // with happens-before semantics.
                results.add(cf);
                return cf;
            });
        }
        return chainedFutures.thenApply(unused ->
            results.stream().map(CompletableFuture::resultNow).toList());
    }
}
