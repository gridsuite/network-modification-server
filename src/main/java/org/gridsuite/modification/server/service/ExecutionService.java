package org.gridsuite.modification.server.service;

import com.powsybl.computation.ComputationManager;
import com.powsybl.computation.local.LocalComputationManager;
import io.micrometer.context.ContextExecutorService;
import io.micrometer.context.ContextSnapshotFactory;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.Getter;
import lombok.SneakyThrows;
import org.springframework.stereotype.Service;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Service
@Getter
public class ExecutionService {

    private ExecutorService executorService;

    private ComputationManager computationManager;

    @SneakyThrows
    @PostConstruct
    private void postConstruct() {
        executorService = ContextExecutorService.wrap(Executors.newCachedThreadPool(),
            () -> ContextSnapshotFactory.builder().build().captureAll());
        computationManager = new LocalComputationManager(getExecutorService());
    }

    @PreDestroy
    private void preDestroy() {
        executorService.shutdown();
    }
}
