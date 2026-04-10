package org.gridsuite.modification.server.config;

import org.springframework.amqp.rabbit.listener.MessageListenerContainer;
import org.springframework.amqp.rabbit.listener.SimpleMessageListenerContainer;
import org.springframework.cloud.stream.config.BindingServiceProperties;
import org.springframework.cloud.stream.config.ListenerContainerCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

@Configuration
public class RabbitConsumerConfiguration {
    /*
     * RabbitMQ consumer priority:
     * https://www.rabbitmq.com/docs/consumer-priority
     *
     * Each container creates exactly one AMQP consumer with prefetch=1 and its own priority.
     * When dispatching messages, RabbitMQ always selects the highest-priority consumer
     * that is available.
     */
    @Bean
    public ListenerContainerCustomizer<MessageListenerContainer> customizer(BindingServiceProperties bindingServiceProperties) {
        /*
         * Using AtomicInteger as in org/springframework/cloud/stream/binder/rabbit/RabbitMessageChannelBinder.java
         * We expect cloud stream to call our customizer exactly once in order for each container so it will produce a sequence of increasing priorities
         */
        AtomicInteger counter = new AtomicInteger(0);
        return (container, destination, group) -> {
            if (container instanceof SimpleMessageListenerContainer smlc && Objects.equals(group, "buildGroup")) {
                smlc.setConsumerArguments(Map.of("x-priority", counter.getAndIncrement()));
            }
        };
    }
}
