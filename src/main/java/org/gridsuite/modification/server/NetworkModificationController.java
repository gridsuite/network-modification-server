/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import java.util.UUID;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/")
@Tag(name = "network-modification-server")
public class NetworkModificationController {

    @Autowired
    private NetworkModificationService networkModificationService;

    @PutMapping(value = "/networks/{networkUuid}/indexes")
    @Operation(summary = "Insert equipment indexes for the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "Equipment indexes have been inserted for the network")})
    public ResponseEntity<Mono<Void>> insertEquipmentIndexes(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid) {
        return ResponseEntity.ok().body(networkModificationService.insertEquipmentIndexes(networkUuid));
    }

    @DeleteMapping(value = "/networks/{networkUuid}/indexes")
    @Operation(summary = "Delete equipment indexes for the network")
    @ApiResponse(responseCode = "200", description = "Equipment indexes have been deleted for the network")
    public ResponseEntity<Mono<Void>> deleteEquipmentIndexes(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid) {
        return ResponseEntity.ok().body(networkModificationService.deleteEquipmentIndexes(networkUuid));
    }

    @PutMapping(value = "/networks/{networkUuid}/switches/{switchId}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change a switch state in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The switch state has been changed")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> changeSwitchState(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam("open") String open) {
        return ResponseEntity.ok().body(networkModificationService.changeSwitchState(networkUuid, switchId, Boolean.parseBoolean(open)));
    }

    @PutMapping(value = "/networks/{networkUuid}/groovy", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change an equipment state in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The equipment state has been changed")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> applyGroovyScript(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                               @RequestBody String groovyScript) {
        return ResponseEntity.ok().body(networkModificationService.applyGroovyScript(networkUuid, groovyScript));
    }

    @GetMapping(value = "/networks/{networkUuid}/modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get modifications list of the default group for a network")
    @ApiResponse(responseCode = "200", description = "List of modifications of the default group for the network")
    public ResponseEntity<Flux<ModificationInfos>> getModifications(@Parameter(description = "Group UUID") @PathVariable("networkUuid") UUID groupUuid) {
        return ResponseEntity.ok().body(networkModificationService.getModifications(groupUuid));
    }

    @GetMapping(value = "/networks/{networkUuid}/elementarymodifications/{modificationUuid}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get an elementary modification of the default group for a network")
    @ApiResponse(responseCode = "200", description = "Elementary modification of the default group for the network")
    public ResponseEntity<Mono<ElementaryModificationInfos>> getElementaryModifications(@Parameter(description = "Group UUID") @PathVariable("networkUuid") UUID groupUuid,
                                                                                        @Parameter(description = "Modification UUID") @PathVariable("modificationUuid") UUID modificationUuid) {
        return ResponseEntity.ok().body(networkModificationService.getElementaryModification(groupUuid, modificationUuid));
    }

    @DeleteMapping(value = "/networks/{networkUuid}/modifications")
    @Operation(summary = "Delete the default modifications group for a network")
    @ApiResponse(responseCode = "200", description = "Default modifications group deleted for the network")
    public ResponseEntity<Mono<Void>> deleteModificationGroup(@Parameter(description = "Group UUID") @PathVariable("networkUuid") UUID groupUuid) {
        return ResponseEntity.ok().body(networkModificationService.deleteModificationGroup(groupUuid));
    }

    @GetMapping(value = "/networks/modificationgroups", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get list of modifications groups")
    @ApiResponse(responseCode = "200", description = "List of modifications groups")
    public ResponseEntity<Flux<UUID>> getModificationGroups() {
        return ResponseEntity.ok().body(networkModificationService.getModificationGroups());
    }

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/status", consumes = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Change the status of a line")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "the line status has been changed")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> changeLineStatus(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Line ID") @PathVariable("lineId") String lineId,
            @RequestBody(required = true) String status) {
        return ResponseEntity.ok().body(networkModificationService.changeLineStatus(networkUuid, lineId, status));
    }
}
