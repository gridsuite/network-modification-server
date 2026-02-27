/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.gridsuite.modification.server.service.NetworkModificationOnCaseService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/cases")
@Tag(name = "network-modification-server on case")
public class NetworkModificationOnCaseController {

    private final NetworkModificationOnCaseService networkModificationOnCaseService;

    public NetworkModificationOnCaseController(NetworkModificationOnCaseService networkModificationOnCaseService) {
        this.networkModificationOnCaseService = networkModificationOnCaseService;
    }

    @PostMapping(value = "/{caseUuid}/network-composite-modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Apply a list of composite modifications on a case")
    @ApiResponse(responseCode = "200", description = "Composite modifications applied on case")
    public ResponseEntity<Void> applyNetworkCompositeModificationsOnCase(@Parameter(description = "Case UUID") @PathVariable("caseUuid") UUID caseUuid,
                                                                         @Parameter(description = "Execution UUID") @RequestParam(name = "executionUuid", required = false) UUID executionUuid,
                                                                         @Parameter(description = "Composite modifications uuids list") @RequestParam("uuids") List<UUID> compositeModificationUuids) {
        networkModificationOnCaseService.applyNetworkCompositeModificationsOnCase(caseUuid, executionUuid, compositeModificationUuids);
        return ResponseEntity.ok().build();
    }
}
