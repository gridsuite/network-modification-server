/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/")
@Tag(name = "network-modification-server")
public class NetworkModificationController {

    private NetworkModificationService networkModificationService;

    public NetworkModificationController(NetworkModificationService networkModificationService) {
        this.networkModificationService = networkModificationService;
    }

    @PutMapping(value = "/networks/{networkUuid}/switches/{switchId}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change a switch state in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The switch state has been changed")})
    public ResponseEntity<Flux<EquipmenModificationInfos>> changeSwitchState(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam(value = "group", required = false) UUID groupUuid,
            @RequestParam("open") String open) {
        return ResponseEntity.ok().body(networkModificationService.changeSwitchState(networkUuid, groupUuid, switchId, Boolean.parseBoolean(open)));
    }

    @PutMapping(value = "/networks/{networkUuid}/groovy", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change an equipment state in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The equipment state has been changed")})
    public ResponseEntity<Flux<EquipmenModificationInfos>> applyGroovyScript(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                                      @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                                      @RequestBody String groovyScript) {
        return ResponseEntity.ok().body(networkModificationService.applyGroovyScript(networkUuid, groupUuid, groovyScript));
    }

    @GetMapping(value = "/groups/{groupUuid}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get modifications list of a group for a network")
    @ApiResponse(responseCode = "200", description = "List of modifications of the group for the network")
    public ResponseEntity<Flux<ModificationInfos>> getModifications(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid) {
        return ResponseEntity.ok().body(networkModificationService.getModifications(groupUuid));
    }

    @DeleteMapping(value = "/groups/{groupUuid}")
    @Operation(summary = "Delete the modifications group for a network")
    @ApiResponse(responseCode = "200", description = "Modifications group deleted for the network")
    public ResponseEntity<Mono<Void>> deleteModificationGroup(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid) {
        return ResponseEntity.ok().body(networkModificationService.deleteModificationGroup(groupUuid));
    }

    @GetMapping(value = "/modificationgroups", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get list of modifications groups")
    @ApiResponse(responseCode = "200", description = "List of modifications groups")
    public ResponseEntity<Flux<UUID>> getModificationGroups() {
        return ResponseEntity.ok().body(networkModificationService.getModificationGroups());
    }

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/status", consumes = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Change the status of a line")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "the line status has been changed")})
    public ResponseEntity<Flux<EquipmenModificationInfos>> changeLineStatus(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Line ID") @PathVariable("lineId") String lineId,
            @RequestParam(value = "group", required = false) UUID groupUuid,
            @RequestBody(required = true) String status) {
        return ResponseEntity.ok().body(networkModificationService.changeLineStatus(networkUuid, groupUuid, lineId, status));
    }

    @PutMapping(value = "/networks/{networkUuid}/loads", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a load in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The load has been created")})
    public ResponseEntity<Flux<EquipmenModificationInfos>> createLoad(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                      @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                      @RequestBody LoadCreationInfos loadCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createLoad(networkUuid, groupUuid, loadCreationInfos));
    }
}
