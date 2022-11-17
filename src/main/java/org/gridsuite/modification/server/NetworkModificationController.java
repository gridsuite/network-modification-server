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
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TYPE_MISMATCH;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/")
@Tag(name = "network-modification-server")
public class NetworkModificationController {

    enum GroupModificationAction {
        MOVE, DUPLICATE
    }

    private NetworkModificationService networkModificationService;

    public NetworkModificationController(NetworkModificationService networkModificationService) {
        this.networkModificationService = networkModificationService;
    }

    @PutMapping(value = "/networks/{networkUuid}/switches/{switchId}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change a switch state in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The switch state has been changed")})
    public ResponseEntity<List<EquipmentModificationInfos>> createSwitchStateModification(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
            @Parameter(description = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam(value = "group", required = false) UUID groupUuid,
            @RequestParam(value = "reportUuid") UUID reportUuid,
            @RequestParam(value = "reporterId") String reporterId,
            @RequestParam("open") String open) {
        return ResponseEntity.ok().body(networkModificationService.createSwitchStateModification(networkUuid, variantId, groupUuid, reportUuid, reporterId, switchId, Boolean.parseBoolean(open)));
    }

    @PutMapping(value = "/networks/{networkUuid}/groovy", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change an equipment state in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The equipment state has been changed"),
                           @ApiResponse(responseCode = "404", description = "the network or equipment not found")})
    public ResponseEntity<List<ModificationInfos>> createGroovyScript(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                      @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                      @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                      @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                      @RequestParam(value = "reporterId") String reporterId,
                                                                      @RequestBody String groovyScript) {
        return ResponseEntity.ok().body(networkModificationService.createGroovyScript(networkUuid, variantId, groupUuid, reportUuid, reporterId, groovyScript));
    }

    @GetMapping(value = "/groups/{groupUuid}/modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get modifications list of a group")
    @ApiResponse(responseCode = "200", description = "List of modifications of the group")
    public ResponseEntity<List<ModificationInfos>> getModifications(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                                    @Parameter(description = "Only metatada") @RequestParam(name = "onlyMetadata", required = false, defaultValue = "false") Boolean onlyMetadata,
                                                                    @Parameter(description = "Return 404 if group is not found or an empty list") @RequestParam(name = "errorOnGroupNotFound", required = false, defaultValue = "true") Boolean errorOnGroupNotFound) {
        return ResponseEntity.ok().body(networkModificationService.getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound));
    }

    @PostMapping(value = "/groups")
    @Operation(summary = "Create a modification group based on another group")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The group and its modifications have been duplicated")})
    public ResponseEntity<Void> createModificationGroup(@RequestParam("groupUuid") UUID groupUuid,
                                                  @RequestParam("duplicateFrom") UUID sourceGroupUuid) {
        networkModificationService.createModificationGroup(sourceGroupUuid, groupUuid);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/groups/{groupUuid}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "For a list of network modifications passed in body, Move them before another one or at the end of the list, or Duplicate them at the end of the list")
    @ApiResponse(responseCode = "200", description = "The modification list of the group has been updated. Missing modifications are returned.")
    public ResponseEntity<List<UUID>> updateModificationGroup(@Parameter(description = "group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                              @Parameter(description = "kind of modification", required = true) @RequestParam(value = "action") GroupModificationAction action,
                                                              @Parameter(description = "the modification Uuid to move before (MOVE option, empty means moving at the end)") @RequestParam(value = "before", required = false) UUID before,
                                                              @RequestBody List<UUID> modificationsUuidList) {
        switch (action) {
            case DUPLICATE:
                return ResponseEntity.ok().body(networkModificationService.duplicateModifications(groupUuid, modificationsUuidList));
            case MOVE:
                networkModificationService.moveModifications(groupUuid, before, modificationsUuidList);
                return ResponseEntity.ok().body(List.of());
            default:
                throw new NetworkModificationException(TYPE_MISMATCH);
        }
    }

    @DeleteMapping(value = "/groups/{groupUuid}")
    @Operation(summary = "Delete the modifications group")
    @ApiResponse(responseCode = "200", description = "Modifications group deleted")
    public ResponseEntity<Void> deleteModificationGroup(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                              @Parameter(description = "Return 404 if group is not found") @RequestParam(name = "errorOnGroupNotFound", required = false, defaultValue = "true") Boolean errorOnGroupNotFound) {
        networkModificationService.deleteModificationGroup(groupUuid, errorOnGroupNotFound);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/groups", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get list of modifications groups")
    @ApiResponse(responseCode = "200", description = "List of modifications groups")
    public ResponseEntity<List<UUID>> getModificationGroups() {
        return ResponseEntity.ok().body(networkModificationService.getModificationGroups());
    }

    @PostMapping(value = "/network-modifications", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create a network modification")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "The network modification was created"),
        @ApiResponse(responseCode = "404", description = "The network or equipment was not found")
    })
    public ResponseEntity<List<? extends ModificationInfos>> createNetworkModification(
            @Parameter(description = "Network ID") @RequestParam("networkUuid") UUID networkUuid,
            @Parameter(description = "Variant ID") @RequestParam(name = "variantId", required = false) String variantId,
            @Parameter(description = "Group ID") @RequestParam(name = "groupUuid", required = false) UUID groupUuid,
            @Parameter(description = "Report ID") @RequestParam("reportUuid") UUID reportUuid,
            @Parameter(description = "Reporter ID") @RequestParam("reporterId") String reporterId,
            @RequestBody ModificationInfos modificationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createNetworkModification(networkUuid, variantId, groupUuid, reportUuid, reporterId, modificationInfos));
    }

    @PutMapping(value = "/network-modifications/{networkModificationUuid}", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Update a network modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The network modification was updated")})
    public ResponseEntity<Void> updateNetworkModification(
            @Parameter(description = "Network modification ID") @PathVariable("networkModificationUuid") UUID networkModificationUuid,
            @RequestBody ModificationInfos modificationInfos) {
        networkModificationService.updateNetworkModification(networkModificationUuid, modificationInfos);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/network-modifications/{networkModificationUuids}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get network modifications")
    @ApiResponse(responseCode = "200", description = "The network modifications were returned")
    public ResponseEntity<List<ModificationInfos>> getNetworkModifications(
            @Parameter(description = "Network modification IDs") @PathVariable(name = "networkModificationUuids") Set<UUID> networkModificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.getNetworkModifications(networkModificationUuids));
    }

    @DeleteMapping(value = "/network-modifications/{networkModificationUuids}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Delete network modifications")
    @ApiResponse(responseCode = "200", description = "The network modifications were deleted")
    public ResponseEntity<Void> deleteNetworkModifications(
            @Parameter(description = "Network modification IDs", required = true) @PathVariable(value = "networkModificationUuids") Set<UUID> networkModificationUuids,
            @Parameter(description = "Group UUID") @RequestParam("groupUuid") UUID groupUuid) {
        networkModificationService.deleteNetworkModifications(groupUuid, networkModificationUuids);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/build")
    @Operation(summary = "Build a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The build has been done")})
    public ResponseEntity<Void> buildVariant(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                     @Parameter(description = "Receiver") @RequestParam(name = "receiver", required = false) String receiver,
                                                     @RequestBody BuildInfos buildInfos) {
        networkModificationService.buildVariant(networkUuid, buildInfos, receiver);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/build/stop")
    @Operation(summary = "Stop a build")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The build has been stopped")})
    public ResponseEntity<Void> stopBuild(@Parameter(description = "Build receiver") @RequestParam(name = "receiver", required = false) String receiver) {
        networkModificationService.stopBuild(receiver);
        return ResponseEntity.ok().build();
    }
}
