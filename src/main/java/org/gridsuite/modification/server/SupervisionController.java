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
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Kevin Le Saulnier <kevin.le-saulnier at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/supervision")
@Tag(name = "network-modification-server - Supervision")
public class SupervisionController {
    private final ModificationApplicationInfosService modificationApplicationInfosService;

    public SupervisionController(ModificationApplicationInfosService modificationApplicationInfosService) {
        this.modificationApplicationInfosService = modificationApplicationInfosService;
    }

    @GetMapping(value = "/network-modifications/reindex")
    @Operation(summary = "reindex all modifications")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Modifications have been reindexed")})
    public ResponseEntity<Void> getIndexedStudiesIndexName() {
        modificationApplicationInfosService.reindexAll();
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/equipments/indexation-count")
    @Operation(summary = "get indexed modifications count")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Indexed modifications count")})
    public ResponseEntity<String> getIndexedModificationsCount() {
        return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(Long.toString(modificationApplicationInfosService.getIndexModificationsCount()));
    }

    @GetMapping(value = "/equipments/to-reindex-count")
    @Operation(summary = "get modifications to reindex count")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Modifications to reindex count")})
    public ResponseEntity<String> getModificationsToReindexCount() {
        return ResponseEntity.ok().contentType(MediaType.TEXT_PLAIN).body(Long.toString(modificationApplicationInfosService.getModificationsToReindexCount()));
    }
}
