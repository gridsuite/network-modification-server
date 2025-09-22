package org.gridsuite.modification.server.service;

import io.micrometer.core.instrument.Gauge;
import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.observation.Observation;
import io.micrometer.observation.Observation.CheckedCallable;
import io.micrometer.observation.Observation.CheckedRunnable;
import io.micrometer.observation.ObservationRegistry;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.gridsuite.modification.ModificationType;
import org.springframework.stereotype.Service;

import java.util.concurrent.ThreadPoolExecutor;

@Service
@RequiredArgsConstructor
public class NetworkModificationObserver {
    private static final String OBSERVATION_PREFIX = "app.network-modification.";
    private static final String MODIFICATION_TYPE_TAG_NAME = "modification_type";

    private static final String TASK_TYPE_TAG_NAME = "type";
    private static final String TASK_TYPE_TAG_VALUE_CURRENT = "current";
    private static final String TASK_TYPE_TAG_VALUE_PENDING = "pending";
    private static final String TASK_POOL_METER_NAME_PREFIX = OBSERVATION_PREFIX + "tasks.pool.";

    @NonNull private final ObservationRegistry observationRegistry;
    @NonNull private final MeterRegistry meterRegistry;

    public <E extends Throwable> void observeApply(final ModificationType modificationType, final CheckedRunnable<E> runnable) throws E {
        this.createObservation("apply", modificationType).observeChecked(runnable);
    }

    public <E extends Throwable> void observeFullBuild(final CheckedRunnable<E> runnable) throws E {
        this.createObservation("consume_build_message").observeChecked(runnable);
    }

    public <R, E extends Throwable> R observeBuild(final BuildExecContext execContext, final CheckedCallable<R, E> callable) throws E {
        Observation observation = this.createObservation("build")
            .highCardinalityKeyValue("network_uuid", execContext.getNetworkUuid().toString());
        //.highCardinalityKeyValue("receiver", execContext.getReceiver())
        if (execContext.getBuildInfos().getOriginVariantId() != null) {
            observation = observation.highCardinalityKeyValue("variant_origin", execContext.getBuildInfos().getOriginVariantId());
        }
        if (execContext.getBuildInfos().getDestinationVariantId() != null) {
            observation = observation.highCardinalityKeyValue("variant_destination", execContext.getBuildInfos().getDestinationVariantId());
        }
        return observation.observeChecked(callable);
    }

    private Observation createObservation(String name, ModificationType modificationType) {
        return Observation.createNotStarted(OBSERVATION_PREFIX + name, observationRegistry)
            .lowCardinalityKeyValue(MODIFICATION_TYPE_TAG_NAME, modificationType.name());
    }

    private Observation createObservation(String name) {
        return Observation.createNotStarted(OBSERVATION_PREFIX + name, observationRegistry);
    }

    public void createThreadPoolMetric(ThreadPoolExecutor threadPoolExecutor) {
        Gauge.builder(TASK_POOL_METER_NAME_PREFIX + TASK_TYPE_TAG_VALUE_CURRENT, threadPoolExecutor, ThreadPoolExecutor::getActiveCount)
            .description("The number of active large network modification tasks in the thread pool")
            .tag(TASK_TYPE_TAG_NAME, TASK_TYPE_TAG_VALUE_CURRENT)
            .register(meterRegistry);
        Gauge.builder(TASK_POOL_METER_NAME_PREFIX + TASK_TYPE_TAG_VALUE_PENDING, threadPoolExecutor, executor -> executor.getQueue().size())
            .description("The number of pending large network modification tasks in the thread pool")
            .tag(TASK_TYPE_TAG_NAME, TASK_TYPE_TAG_VALUE_PENDING)
            .register(meterRegistry);
    }
}
