/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.ReportNodeDeserializer;
import com.powsybl.commons.report.ReportNodeJsonModule;
import org.gridsuite.modification.server.dto.ReportMode;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.Objects;
import java.util.UUID;

/**
 * @author Slimane amar <slimane.amar at rte-france.com>
 */
@Service
public class ReportService {
    private static final String REPORT_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    private String reportServerBaseUri;

    private RestClient reportServerRest;

    private final ObjectMapper objectMapper;

    public ReportService(@Value("${gridsuite.services.report-server.base-uri:http://report-server}") String reportServerURI,
                         ObjectMapper objectMapper,
                         RestClient restClient) {
        this.reportServerBaseUri = reportServerURI;
        this.objectMapper = objectMapper;
        this.objectMapper.registerModule(new ReportNodeJsonModule());
        this.objectMapper.setInjectableValues(new InjectableValues.Std().addValue(ReportNodeDeserializer.DICTIONARY_VALUE_ID, null));
        this.reportServerRest = restClient;
    }

    public void setReportServerBaseUri(String reportServerBaseUri) {
        this.reportServerBaseUri = reportServerBaseUri;
    }

    private String getReportServerURI() {
        return this.reportServerBaseUri + DELIMITER + REPORT_API_VERSION + DELIMITER + "reports" + DELIMITER;
    }

    public void setReportServerRest(RestClient reportServerRest) {
        this.reportServerRest = Objects.requireNonNull(reportServerRest, "reportServerRest can't be null");
    }

    public void sendReport(UUID reportUuid, ReportNode reportNode, ReportMode reportMode) {
        var path = UriComponentsBuilder.fromPath("{reportUuid}{endpoint}")
            .buildAndExpand(reportUuid, reportMode == ReportMode.REPLACE ? "/replace" : "")
            .toUriString();
        try {
            reportServerRest.put()
                .uri(this.getReportServerURI() + path)
                .contentType(MediaType.APPLICATION_JSON)
                .body(objectMapper.writeValueAsString(reportNode))
                .retrieve()
                .toEntity(ReportNode.class);
        } catch (JsonProcessingException error) {
            throw new PowsyblException("error creating report", error);
        }
    }

    public void sendReport(UUID reportUuid, ReportNode reportNode) {
        sendReport(reportUuid, reportNode, ReportMode.APPEND);
    }
}
