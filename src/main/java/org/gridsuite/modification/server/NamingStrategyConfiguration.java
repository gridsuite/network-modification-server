/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server;

import com.powsybl.iidm.modification.topology.DefaultNamingStrategy;
import com.powsybl.iidm.modification.topology.NamingStrategiesServiceLoader;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Configuration
public class NamingStrategyConfiguration {

    private final NamingStrategiesServiceLoader namingStrategiesServiceLoader = new NamingStrategiesServiceLoader();

    @Value("${naming-strategy:Default}")
    private String namingStrategy;

    @Bean
    public NamingStrategy getNamingStrategy() {
        return namingStrategiesServiceLoader.findNamingStrategyByName(namingStrategy).orElse(new DefaultNamingStrategy());
    }
}
