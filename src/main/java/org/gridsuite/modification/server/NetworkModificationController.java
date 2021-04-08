/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import java.util.Set;
import java.util.UUID;

import io.swagger.annotations.*;
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
@Api(tags = "network-modification-server")
public class NetworkModificationController {

    @Autowired
    private NetworkModificationService networkModificationService;

    @PutMapping(value = "/networks/{networkUuid}/switches/{switchId}", produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "change a switch state in the network", response = Set.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The switch state has been changed")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> changeSwitchState(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam("open") String open) {
        return ResponseEntity.ok().body(networkModificationService.changeSwitchState(networkUuid, switchId, Boolean.parseBoolean(open)));
    }

    @PutMapping(value = "/networks/{networkUuid}/groovy", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "change an equipment state in the network", response = Set.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The equipment state has been changed")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> applyGroovyScript(@ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                               @RequestBody String groovyScript) {
        return ResponseEntity.ok().body(networkModificationService.applyGroovyScript(networkUuid, groovyScript));
    }

    @GetMapping(value = "/networks/{networkUuid}/modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Get modifications list of the default group for a network")
    @ApiResponse(code = 200, message = "List of modifications of the default group for the network")
    public ResponseEntity<Flux<ModificationInfos>> getModifications(@ApiParam(value = "Group UUID") @PathVariable("networkUuid") UUID groupUuid) {
        return ResponseEntity.ok().body(networkModificationService.getModifications(groupUuid));
    }

    @GetMapping(value = "/networks/{networkUuid}/elementarymodifications/{modificationUuid}", produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Get an elementary modification of the default group for a network")
    @ApiResponse(code = 200, message = "Elementary modification of the default group for the network")
    public ResponseEntity<Mono<ElementaryModificationInfos>> getElementaryModifications(@ApiParam(value = "Group UUID") @PathVariable("networkUuid") UUID groupUuid,
                                                                                        @ApiParam(value = "Modification UUID") @PathVariable("modificationUuid") UUID modificationUuid) {
        return ResponseEntity.ok().body(networkModificationService.getElementaryModification(groupUuid, modificationUuid));
    }

    @DeleteMapping(value = "/networks/{networkUuid}/modifications")
    @ApiOperation(value = "Delete the default modifications group for a network")
    @ApiResponse(code = 200, message = "Default modifications group deleted for the network")
    public ResponseEntity<Mono<Void>> deleteModificationGroup(@ApiParam(value = "Group UUID") @PathVariable("networkUuid") UUID groupUuid) {
        return ResponseEntity.ok().body(networkModificationService.deleteModificationGroup(groupUuid));
    }

    @GetMapping(value = "/networks/modificationgroups", produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Get list of modifications groups")
    @ApiResponse(code = 200, message = "List of modifications groups")
    public ResponseEntity<Flux<UUID>> getModificationGroups() {
        return ResponseEntity.ok().body(networkModificationService.getModificationGroups());
    }

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/lockout")
    @ApiOperation(value = "lockout a line")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "the line has been locked-out")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> lockoutLine(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Line ID") @PathVariable("lineId") String lineId) {
        return ResponseEntity.ok().body(networkModificationService.lockoutLine(networkUuid, lineId));
    }

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/trip")
    @ApiOperation(value = "trip a line")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "the line has been tripped")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> tripLine(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Line ID") @PathVariable("lineId") String lineId) {
        return ResponseEntity.ok().body(networkModificationService.tripLine(networkUuid, lineId));
    }

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/energiseEnd")
    @ApiOperation(value = "energise one end of a line")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "the line end has been energised")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> energiseLineEnd(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Line ID") @PathVariable("lineId") String lineId,
            @RequestParam("side") String side) {
        return ResponseEntity.ok().body(networkModificationService.energiseLineEnd(networkUuid, lineId, side));
    }

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/switchOn")
    @ApiOperation(value = "switch on a line")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "the line has been locked-out")})
    public ResponseEntity<Flux<ElementaryModificationInfos>> switchOnLine(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Line ID") @PathVariable("lineId") String lineId) {
        return ResponseEntity.ok().body(networkModificationService.switchOnLine(networkUuid, lineId));
    }
}
