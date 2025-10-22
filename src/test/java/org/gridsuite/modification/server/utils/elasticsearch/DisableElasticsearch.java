/*
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils.elasticsearch;

import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.TombstonedEquipmentInfosRepository;
import org.junit.jupiter.api.Tag;
import org.mockito.Mockito;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.test.context.TestPropertySource;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@TestPropertySource(properties = {
    "spring.autoconfigure.exclude=org.springframework.boot.autoconfigure.data.elasticsearch.ElasticsearchRepositoriesAutoConfiguration"
})
@Import(DisableElasticsearch.MockConfig.class)
@Tag("Docker")
public @interface DisableElasticsearch {

    @TestConfiguration(proxyBeanMethods = false)
    class MockConfig {
        @Bean
        public EmbeddedElasticsearch embeddedElasticsearch() {
            return Mockito.mock(EmbeddedElasticsearch.class);
        }

        @Bean
        public EquipmentInfosRepository equipmentInfosRepository() {
            return Mockito.mock(EquipmentInfosRepository.class);
        }

        @Bean
        public ModificationApplicationInfosRepository modificationApplicationInfosRepository() {
            return Mockito.mock(ModificationApplicationInfosRepository.class);
        }

        @Bean
        public TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository() {
            return Mockito.mock(TombstonedEquipmentInfosRepository.class);
        }

        @Bean
        public ElasticsearchOperations elasticsearchOperations() {
            return Mockito.mock(ElasticsearchOperations.class);
        }
    }

}
