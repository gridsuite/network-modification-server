/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import com.powsybl.network.store.client.RestClientImpl;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.http.client.ClientHttpRequestFactoryBuilder;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuppressWarnings("checkstyle:HideUtilityClassConstructor")
@SpringBootApplication(scanBasePackageClasses = { NetworkModificationApplication.class, NetworkStoreService.class })
public class NetworkModificationApplication {
    public static void main(String[] args) {
        SpringApplication.run(NetworkModificationApplication.class, args);
    }


    // Override NetworkStoreService defined in the lib network-store-client
    // because it always autodetects the resttemplate httpclient from the classpath
    // instead of using the resttemplatebuilder that can be controlled by runtime configuration/
    @Primary
    @Bean
    public NetworkStoreService networkstoreservice(
        @Value("${powsybl.services.network-store-server.base-uri:http://network-store-server/}") String baseUri,
        @Value("${powsybl.services.network-store-server.preloading-strategy:NONE}") PreloadingStrategy defaultPreloadingStrategy
    ) {
        RestTemplateBuilder builder = RestClientImpl.createRestTemplateBuilder(baseUri);
        builder = builder.requestFactoryBuilder(ClientHttpRequestFactoryBuilder.simple());
        RestClientImpl restClient = new RestClientImpl(builder);
        return new NetworkStoreService(restClient, defaultPreloadingStrategy);
    }
}
