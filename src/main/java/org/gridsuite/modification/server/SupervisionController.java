/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.apache.http.HttpHost;
import org.elasticsearch.client.RestClient;
import org.gridsuite.modification.server.service.SupervisionService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Kevin Le Saulnier <kevin.le-saulnier at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/supervision")
@Tag(name = "network-modification-server - Supervision")
public class SupervisionController {
    private final SupervisionService supervisionService;

    private final RestClient restClient;

    public SupervisionController(SupervisionService supervisionService,
                                 RestClient restClient) {
        this.supervisionService = supervisionService;
        this.restClient = restClient;
    }

    @GetMapping(value = "/elasticsearch-host")
    @Operation(summary = "get the elasticsearch address")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "the elasticsearch address")})
    public ResponseEntity<String> getElasticsearchHost() {
        HttpHost httpHost = restClient.getNodes().get(0).getHost();
        String host = httpHost.getHostName()
            + ":"
            + httpHost.getPort();
        return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(host);
    }

    @PostMapping(value = "/network-modifications/reindex")
    @Operation(summary = "reindex all modifications")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Modifications have been reindexed")})
    public ResponseEntity<Void> reindexAllModifications() {
        supervisionService.reindexAll();
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/network-modifications/index")
    @Operation(summary = "Recreate modifications index")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Elasticsearch modifications index recreated successfully"),
        @ApiResponse(responseCode = "500", description = "Failed to recreate Elasticsearch index")
    })
    public ResponseEntity<Void> recreateESIndex() {
        supervisionService.recreateIndex();
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/network-modifications/indexation-count")
    @Operation(summary = "get indexed modifications count")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Indexed modifications count")})
    public ResponseEntity<String> getIndexedModificationsCount() {
        return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(Long.toString(supervisionService.getIndexModificationsCount()));
    }

    @GetMapping(value = "/network-modifications/to-reindex-count")
    @Operation(summary = "get modifications to reindex count")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Modifications to reindex count")})
    public ResponseEntity<String> getModificationsToReindexCount() {
        return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(Long.toString(supervisionService.getModificationsToReindexCount()));
    }

    @GetMapping(value = "/network-modifications/index-name")
    @Operation(summary = "get the modifications index name")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Modifications index name")})
    public ResponseEntity<String> getModificationsIndexName() {
        return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(supervisionService.getModificationIndexName());
    }
}
