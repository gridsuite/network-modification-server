package org.gridsuite.modification.server.service;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.MultiGauge;
import io.micrometer.core.instrument.Tags;
import io.micrometer.observation.Observation;
import io.micrometer.observation.ObservationRegistry;
import lombok.NonNull;
import org.gridsuite.modification.ModificationType;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.ThreadPoolExecutor;

@Service
public class NetworkModificationObserver {
    private static final String OBSERVATION_PREFIX = "app.network-modification.";
    private static final String MODIFICATION_TYPE_TAG_NAME = "modification_type";

    private static final String TASK_TYPE_TAG_NAME = "type";
    private static final String TASK_TYPE_TAG_VALUE_CURRENT = "current";
    private static final String TASK_TYPE_TAG_VALUE_PENDING = "pending";
    private static final String TASK_POOL_METER_NAME = OBSERVATION_PREFIX + "tasks.pool";

    private final ObservationRegistry observationRegistry;
    private final MeterRegistry meterRegistry;

    public NetworkModificationObserver(@NonNull ObservationRegistry observationRegistry, @NonNull MeterRegistry meterRegistry) {
        this.observationRegistry = observationRegistry;
        this.meterRegistry = meterRegistry;
    }

    public <E extends Throwable> void observe(String name, ModificationType modificationType, Observation.CheckedRunnable<E> runnable) throws E {
        createObservation(name, modificationType).observeChecked(runnable);
    }

    private Observation createObservation(String name, ModificationType modificationType) {
        return Observation.createNotStarted(OBSERVATION_PREFIX + name, observationRegistry)
            .lowCardinalityKeyValue(MODIFICATION_TYPE_TAG_NAME, modificationType.name());
    }

    public void createThreadPoolMetric(ThreadPoolExecutor threadPoolExecutor) {
        MultiGauge multiGauge = MultiGauge.builder(TASK_POOL_METER_NAME).description("The number of large network modifications (tasks) in the thread pool").register(meterRegistry);
        multiGauge.register(List.of(
            MultiGauge.Row.of(Tags.of(TASK_TYPE_TAG_NAME, TASK_TYPE_TAG_VALUE_CURRENT), threadPoolExecutor::getActiveCount),
            MultiGauge.Row.of(Tags.of(TASK_TYPE_TAG_NAME, TASK_TYPE_TAG_VALUE_PENDING), () -> threadPoolExecutor.getQueue().size())
        ));
    }
}
