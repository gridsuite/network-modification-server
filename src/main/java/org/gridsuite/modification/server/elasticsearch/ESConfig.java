/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.elasticsearch.client.RestHighLevelClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.elasticsearch.client.ClientConfiguration;
import org.springframework.data.elasticsearch.client.RestClients;
import org.springframework.data.elasticsearch.config.AbstractElasticsearchConfiguration;
import org.springframework.data.elasticsearch.repository.config.EnableElasticsearchRepositories;

import javax.annotation.Nonnull;
import java.net.InetSocketAddress;

/**
 * A class to configure DB elasticsearch client for indexation
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */

@Configuration
@EnableElasticsearchRepositories
@Lazy
public class ESConfig extends AbstractElasticsearchConfiguration {

    @Value("#{'${spring.data.elasticsearch.embedded:false}' ? 'localhost' : '${spring.data.elasticsearch.host}'}")
    private String esHost;

    @Value("#{'${spring.data.elasticsearch.embedded:false}' ? '${spring.data.elasticsearch.embedded.port:}' : '${spring.data.elasticsearch.port}'}")
    private int esPort;

    @Value("${spring.data.elasticsearch.client.timeout:60}")
    int timeout;

    @Bean
    @ConditionalOnExpression("'${spring.data.elasticsearch.enabled:false}' == 'true'")
    public EquipmentInfosService equipmentInfosServiceImpl(EquipmentInfosRepository equipmentInfosRepository, TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository) {
        return new EquipmentInfosServiceImpl(equipmentInfosRepository, tombstonedEquipmentInfosRepository);
    }

    @Bean
    @ConditionalOnExpression("'${spring.data.elasticsearch.enabled:false}' == 'false'")
    public EquipmentInfosService equipmentInfosServiceMock() {
        return new EquipmentInfosServiceMock();
    }

    @Bean
    @Override
    @SuppressWarnings("squid:S2095")
    @Nonnull
    public RestHighLevelClient elasticsearchClient() {
        ClientConfiguration clientConfiguration = ClientConfiguration.builder()
            .connectedTo(InetSocketAddress.createUnresolved(esHost, esPort))
            .withConnectTimeout(timeout * 1000L).withSocketTimeout(timeout * 1000L)
            .build();

        return RestClients.create(clientConfiguration).rest();
    }
}
