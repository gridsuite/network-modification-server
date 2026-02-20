/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.datasource.MemDataSource;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.server.dto.ReportMode;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class NetworkModificationOnCaseService {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationOnCaseService.class);

    private final NetworkModificationRepository networkModificationRepository;

    private final NetworkConversionService networkConversionService;

    private final FilterService filterService;

    private final ReportService reportService;

    private final NotificationService notificationService;

    private final RestTemplate restTemplate;

    private final String caseExportFormat = "XIIDM";

    public NetworkModificationOnCaseService(NetworkModificationRepository networkModificationRepository,
                                            NetworkConversionService networkConversionService,
                                            FilterService filterService,
                                            NotificationService notificationService,
                                            ReportService reportService,
                                            RestTemplateBuilder restTemplateBuilder,
                                            @Value("${powsybl.services.case-server.base-uri:http://case-server}") String caseServerBaseUri) {
        this.networkModificationRepository = networkModificationRepository;
        this.networkConversionService = networkConversionService;
        this.filterService = filterService;
        this.notificationService = notificationService;
        this.reportService = reportService;
        this.restTemplate = restTemplateBuilder.rootUri(caseServerBaseUri).build();
    }

    private Network loadNetworkFromCase(UUID caseUuid, ReportNode reportNode) {
        return networkConversionService.createNetwork(caseUuid, reportNode);
    }

    private List<ModificationInfos> getModificationsFromCompositeModifications(List<UUID> compositeModificationUuids) {
        return networkModificationRepository.getCompositeModificationsInfos(compositeModificationUuids);
    }

    private void applyModifications(Network network, List<ModificationInfos> modificationsInfos, ReportNode reportNode, FilterService filterService) {
        modificationsInfos.stream()
            .filter(ModificationInfos::getActivated)
            .forEach(modificationInfos -> {
                try {
                    AbstractModification modification = modificationInfos.toModification();
                    modification.check(network);
                    modification.initApplicationContext(filterService, null);
                    modification.apply(network, reportNode);
                } catch (Exception e) {
                    // For now, we just log the error, and we continue to apply the following modifications
                    handleException(modificationInfos.getErrorType(), e);
                }
            });
    }

    private UUID save(Resource resource) {
        String uri = "/v1/cases";

        MultiValueMap<String, Object> body = new LinkedMultiValueMap<>();
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.MULTIPART_FORM_DATA);
        body.add("file", resource);
        HttpEntity<MultiValueMap<String, Object>> request = new HttpEntity<>(body, headers);

        return restTemplate.postForObject(uri, request, UUID.class);
    }

    private UUID save(Network network) throws IOException {
        MemDataSource memDataSource = new MemDataSource();
        network.write(this.caseExportFormat, null, memDataSource);

        Set<String> listNames = memDataSource.listNames(".*");
        String caseFileName = "apply-modifications-output." + this.caseExportFormat.toLowerCase();
        return save(new ByteArrayResource(memDataSource.getData(listNames.toArray()[0].toString())) {
            @Override
            public String getFilename() {
                return caseFileName;
            }
        });
    }

    private void handleException(NetworkModificationException.Type typeIfError, Exception e) {
        boolean isApplicationException = PowsyblException.class.isAssignableFrom(e.getClass());
        if (!isApplicationException) {
            LOGGER.error("{}", e.getMessage(), e);
        } else {
            LOGGER.error("{} : {}", typeIfError.name(), e.getMessage(), e);
        }
    }

    public void applyNetworkCompositeModificationsOnCase(UUID caseUuid, UUID executionUuid, List<UUID> compositeModificationUuids) {
        UUID resultCaseUuid = null;
        UUID reportUuid = null;
        String status = "COMPLETED";

        try {
            ReportNode rootReport = ReportNode.newRootReportNode()
                .withAllResourceBundlesFromClasspath()
                .withMessageTemplate("network.modification.server.caseUuid")
                .withUntypedValue("caseUuid", caseUuid.toString())
                .build();

            LOGGER.info("Applying modifications on case {}", caseUuid);

            // create network from case
            Network network = loadNetworkFromCase(caseUuid, rootReport);

            // get modifications from composite modifications
            List<ModificationInfos> modifications = getModificationsFromCompositeModifications(compositeModificationUuids);

            // apply modifications
            applyModifications(network, modifications, rootReport, filterService);

            // send report to report server
            reportUuid = UUID.randomUUID();
            reportService.sendReport(reportUuid, rootReport, ReportMode.APPEND);

            // save network in case server
            resultCaseUuid = save(network);
        } catch (Exception e) {
            status = "FAILED";
        } finally {
            notificationService.sendMessage(MessageBuilder.withPayload(new CaseResultInfos(resultCaseUuid, executionUuid, reportUuid, null, "APPLY_MODIFICATIONS", status)).build(), "publishCaseResult-out-0");
        }
    }
}
