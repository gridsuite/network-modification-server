package org.gridsuite.modification.server.service;

import io.micrometer.observation.Observation;
import io.micrometer.observation.ObservationRegistry;
import lombok.NonNull;
import org.gridsuite.modification.ModificationType;
import org.springframework.stereotype.Service;

@Service
public class NetworkModificationObserver {
    protected static final String OBSERVATION_PREFIX = "app.network-modification.";
    protected static final String MODIFICATION_TYPE_TAG_NAME = "modification_type";

    private final ObservationRegistry observationRegistry;

    public NetworkModificationObserver(@NonNull ObservationRegistry observationRegistry) {
        this.observationRegistry = observationRegistry;
    }

    public <E extends Throwable> void observe(String name, ModificationType modificationType, Observation.CheckedRunnable<E> runnable) throws E {
        createObservation(name, modificationType).observeChecked(runnable);
    }

    private Observation createObservation(String name, ModificationType modificationType) {
        return Observation.createNotStarted(OBSERVATION_PREFIX + name, observationRegistry)
            .lowCardinalityKeyValue(MODIFICATION_TYPE_TAG_NAME, modificationType.name());
    }

}
