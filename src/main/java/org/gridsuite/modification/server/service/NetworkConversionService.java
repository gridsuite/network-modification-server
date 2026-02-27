/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.cases.datasource.CaseDataSourceClient;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.computation.local.LocalComputationManager;
import com.powsybl.iidm.network.Importer;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.NetworkFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.Properties;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class NetworkConversionService {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkConversionService.class);

    private final RestTemplate caseServerRest;

    public NetworkConversionService(@Value("${powsybl.services.case-server.base-uri:http://case-server/}") String caseServerBaseUri,
                                    RestTemplateBuilder restTemplateBuilder) {
        this.caseServerRest = restTemplateBuilder.rootUri(caseServerBaseUri).build();
    }

    public Network createNetwork(UUID caseUuid, ReportNode reporter) {
        LOGGER.info("Creating network");

        CaseDataSourceClient dataSource = new CaseDataSourceClient(caseServerRest, caseUuid);

        Importer importer = Importer.find(dataSource, LocalComputationManager.getDefault());
        if (importer == null) {
            throw new PowsyblException("No importer found");
        } else {
            return importer.importData(dataSource, NetworkFactory.findDefault(), new Properties(), reporter);
        }
    }
}
