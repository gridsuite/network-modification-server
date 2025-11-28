/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import io.micrometer.context.ContextExecutorService;
import io.micrometer.context.ContextRegistry;
import io.micrometer.context.ThreadLocalAccessor;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.lang.reflect.Field;
import java.util.concurrent.ExecutorService;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/**
 * @author Mohamed Benrejeb <mohamed.ben-rejeb at rte-france.com>
 */
@SpringBootTest
class LargeNetworkModificationExecutionServiceTest {
    private static final String THREAD_LOCAL_KEY = "network-modification-thread-local";
    private final ThreadLocal<String> threadLocal = new ThreadLocal<>();

    @Autowired
    LargeNetworkModificationExecutionService service;

    @Test
    void supplyAsyncPropagatesContext() throws Exception {
        ContextRegistry.getInstance().registerThreadLocalAccessor(new ThreadLocalAccessor<String>() {
            @Override
            public String key() {
                return THREAD_LOCAL_KEY;
            }

            @Override
            public String getValue() {
                return threadLocal.get();
            }

            @Override
            public void setValue(String value) {
                threadLocal.set(value);
            }

            @Override
            public void setValue() {
                threadLocal.remove();
            }
        });

        Field executorField = LargeNetworkModificationExecutionService.class.getDeclaredField("executorService");
        executorField.setAccessible(true);
        ExecutorService executorService = (ExecutorService) executorField.get(service);

        threadLocal.set("expected-context");

        assertInstanceOf(ContextExecutorService.class, executorService, "executor should be wrapped in ContextExecutorService");
        assertEquals("expected-context", executorService.submit(threadLocal::get).get());
    }
}
