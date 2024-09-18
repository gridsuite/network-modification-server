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
import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;
import org.gridsuite.modification.server.service.LineTypesCatalogService;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;
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
        MOVE, COPY, INSERT
    }

    private final NetworkModificationService networkModificationService;

    private final LineTypesCatalogService lineTypesCatalogService;

    public NetworkModificationController(NetworkModificationService networkModificationService,
            LineTypesCatalogService lineTypesCatalogService) {
        this.networkModificationService = networkModificationService;
        this.lineTypesCatalogService = lineTypesCatalogService;
    }

    @GetMapping(value = "/groups/{groupUuid}/network-modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get modifications list of a group")
    @ApiResponse(responseCode = "200", description = "List of modifications of the group")
    public ResponseEntity<List<ModificationInfos>> getNetworkModifications(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                                           @Parameter(description = "Only metadata") @RequestParam(name = "onlyMetadata", required = false, defaultValue = "false") Boolean onlyMetadata,
                                                                           @Parameter(description = "Stashed modifications") @RequestParam(name = "onlyStashed", required = false, defaultValue = "false") Boolean onlyStashed,
                                                                           @Parameter(description = "Return 404 if group is not found or an empty list") @RequestParam(name = "errorOnGroupNotFound", required = false, defaultValue = "true") Boolean errorOnGroupNotFound) {
        return ResponseEntity.ok().body(networkModificationService.getNetworkModifications(groupUuid, onlyMetadata, errorOnGroupNotFound, onlyStashed));
    }

    @GetMapping(value = "/groups/{groupUuid}/network-modifications-count", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get a groups's modification count")
    @ApiResponse(responseCode = "200", description = "Count of group's modifications")
    public ResponseEntity<Integer> getNetworkModificationsCount(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                                @Parameter(description = "Stashed modifications") @RequestParam(name = "stashed", required = false, defaultValue = "false") Boolean stashed) {
        return ResponseEntity.ok().body(networkModificationService.getNetworkModificationsCount(groupUuid, stashed));
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
    @ApiResponse(responseCode = "200", description = "The modification list of the group has been updated.")
    public ResponseEntity<Optional<NetworkModificationResult>> handleNetworkModifications(@Parameter(description = "updated group UUID, where modifications are pasted") @PathVariable("groupUuid") UUID targetGroupUuid,
                                                                                          @Parameter(description = "kind of modification", required = true) @RequestParam(value = "action") GroupModificationAction action,
                                                                                          @Parameter(description = "the network uuid", required = true) @RequestParam(value = "networkUuid") UUID networkUuid,
                                                                                          @Parameter(description = "the report uuid", required = true) @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                                          @Parameter(description = "the reporter id", required = true) @RequestParam(value = "reporterId") UUID reporterId,
                                                                                          @Parameter(description = "the variant id", required = true) @RequestParam(value = "variantId") String variantId,
                                                                                          @Parameter(description = "the modification Uuid to move before (MOVE option, empty means moving at the end)") @RequestParam(value = "before", required = false) UUID beforeModificationUuid,
                                                                                          @Parameter(description = "origin group UUID, where modifications are copied or cut") @RequestParam(value = "originGroupUuid", required = false) UUID originGroupUuid,
                                                                                          @Parameter(description = "destination node can be built (default is true)") @RequestParam(value = "build", required = false, defaultValue = "true") Boolean build,
                                                                                          @RequestBody List<UUID> modificationsUuidList) {
        switch (action) {
            case COPY:
                return ResponseEntity.ok().body(networkModificationService.duplicateModifications(targetGroupUuid, networkUuid, variantId, new ReportInfos(reportUuid, reporterId.toString()), modificationsUuidList));
            case INSERT:
                return ResponseEntity.ok().body(networkModificationService.insertCompositeModifications(targetGroupUuid, networkUuid, variantId, new ReportInfos(reportUuid, reporterId.toString()), modificationsUuidList));
            case MOVE:
                UUID sourceGroupUuid = originGroupUuid == null ? targetGroupUuid : originGroupUuid;
                boolean canBuildNode = build;
                if (sourceGroupUuid.equals(targetGroupUuid)) {
                    canBuildNode = false;
                }
                return ResponseEntity.ok().body(networkModificationService.moveModifications(targetGroupUuid, sourceGroupUuid, beforeModificationUuid, networkUuid, variantId, new ReportInfos(reportUuid, reporterId.toString()), modificationsUuidList, canBuildNode));
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

    @PostMapping(value = "/network-modifications", params = {"networkUuid", "reportUuid", "reporterId"}, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create a network modification")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "The network modification was created"),
        @ApiResponse(responseCode = "404", description = "The network or equipment was not found")})
    public ResponseEntity<Optional<NetworkModificationResult>> createNetworkModification(
            @Parameter(description = "Network UUID") @RequestParam("networkUuid") UUID networkUuid,
            @Parameter(description = "Variant ID") @RequestParam(name = "variantId", required = false) String variantId,
            @Parameter(description = "Group UUID") @RequestParam(name = "groupUuid", required = false) UUID groupUuid,
            @Parameter(description = "Report UUID") @RequestParam("reportUuid") UUID reportUuid,
            @Parameter(description = "Reporter ID") @RequestParam("reporterId") String reporterId,
            @RequestBody ModificationInfos modificationInfos) {
        modificationInfos.check();
        return ResponseEntity.ok().body(networkModificationService.createNetworkModification(networkUuid, variantId, groupUuid, new ReportInfos(reportUuid, reporterId), modificationInfos));
    }

    @PutMapping(value = "/network-modifications/{uuid}", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Update a network modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The network modification was updated")})
    public ResponseEntity<Void> updateNetworkModification(
            @Parameter(description = "Network modification UUID") @PathVariable("uuid") UUID networkModificationUuid,
            @RequestBody ModificationInfos modificationInfos) {
        networkModificationService.updateNetworkModification(networkModificationUuid, modificationInfos);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/network-modifications/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get a network modification")
    @ApiResponse(responseCode = "200", description = "The network modifications were returned")
    public ResponseEntity<ModificationInfos> getNetworkModification(
            @Parameter(description = "Network modification UUID") @PathVariable("uuid") UUID networkModificationUuid) {
        return ResponseEntity.ok().body(networkModificationService.getNetworkModification(networkModificationUuid));
    }

    @DeleteMapping(value = "/network-modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Delete network modifications. If owned by a group, then groupUuid must be provided.")
    @ApiResponse(responseCode = "200", description = "The network modifications were deleted")
    public ResponseEntity<Void> deleteNetworkModifications(
            @Parameter(description = "Network modification UUIDs") @RequestParam(name = "uuids", required = false) List<UUID> networkModificationUuids,
            @Parameter(description = "Group UUID") @RequestParam(name = "groupUuid", required = false) UUID groupUuid) {
        networkModificationService.deleteNetworkModifications(groupUuid, networkModificationUuids);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/build")
    @Operation(summary = "Build a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The build has been done")})
    public ResponseEntity<Void> buildVariant(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                     @Parameter(description = "Receiver") @RequestParam(name = "receiver", required = false) String receiver,
                                                     @RequestBody BuildInfos buildInfos) {
        networkModificationService.buildVariantRequest(networkUuid, buildInfos, receiver);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/build/stop")
    @Operation(summary = "Stop a build")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The build has been stopped")})
    public ResponseEntity<Void> stopBuild(@Parameter(description = "Build receiver") @RequestParam(name = "receiver", required = false) String receiver) {
        networkModificationService.stopBuildRequest(receiver);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/network-modifications/catalog/line_types", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get a line types catalog")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line types catalog is returned")})
    public ResponseEntity<List<LineTypeInfos>> getLineTypes() {
        return ResponseEntity.ok().body(lineTypesCatalogService.getAllLineTypes());
    }

    @PostMapping(value = "/network-modifications/catalog/line_types", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create or reset completely a line types catalog")
    @ApiResponse(responseCode = "200", description = "The line types catalog is created or reset")
    public ResponseEntity<Void> resetLineTypes(@RequestBody List<LineTypeInfos> lineTypes) {
        lineTypesCatalogService.resetLineTypes(lineTypes);
        return ResponseEntity.ok().build();
    }

    @DeleteMapping(value = "/network-modifications/catalog/line_types", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Delete line types catalog")
    @ApiResponse(responseCode = "200", description = "The line types catalog is deleted")
    public ResponseEntity<Void> deleteLineTypesCatalog() {
        lineTypesCatalogService.deleteLineTypesCatalog();
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/groups/modification", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create a group containing a modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The group with the modification has been created")})
    public ResponseEntity<UUID> createModificationInGroup(@RequestBody ModificationInfos modificationsInfos) {
        return ResponseEntity.ok().body(networkModificationService.createModificationInGroup(modificationsInfos));
    }

    @PostMapping(value = "/network-composite-modifications", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create a network composite modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The composite modification has been created")})
    public ResponseEntity<UUID> createNetworkCompositeModification(@RequestBody List<UUID> modificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.createNetworkCompositeModification(modificationUuids));
    }

    @PostMapping(value = "/network-modifications", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Duplicate some modifications without group ownership")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The duplicated modifications uuids mapped with their source uuid")})
    public ResponseEntity<Map<UUID, UUID>> duplicateModifications(@Parameter(description = "source modifications uuids list to duplicate") @RequestBody List<UUID> sourceModificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.duplicateModifications(sourceModificationUuids));
    }

    @PutMapping(value = "/network-modifications", produces = MediaType.APPLICATION_JSON_VALUE, params = "stashed")
    @Operation(summary = "stash or unstash network modifications")
    @ApiResponse(responseCode = "200", description = "The network modifications were stashed")
    public ResponseEntity<Void> stashNetworkModifications(
            @Parameter(description = "Network modification UUIDs") @RequestParam("uuids") List<UUID> networkModificationUuids,
            @Parameter(description = "Group UUID") @RequestParam("groupUuid") UUID groupUuid,
            @Parameter(description = "stash or unstash network modifications") @RequestParam(name = "stashed", defaultValue = "true") Boolean stashed) {
        if (Boolean.TRUE.equals(stashed)) {
            networkModificationService.stashNetworkModifications(networkModificationUuids);
        } else {
            networkModificationService.restoreNetworkModifications(networkModificationUuids);
        }
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/network-modifications", produces = MediaType.APPLICATION_JSON_VALUE, params = "activated")
    @Operation(summary = "activate or deactivate network modifications")
    @ApiResponse(responseCode = "200", description = "The activation status related to the network modification was successfully updated")
    public ResponseEntity<Void> updateNetworkModificationsActivationStatus(
        @Parameter(description = "Network modification UUIDs") @RequestParam("uuids") List<UUID> networkModificationUuids,
        @Parameter(description = "activate or deactivate network modifications") @RequestParam(name = "activated") Boolean activated) {
        networkModificationService.updateNetworkModificationActivation(networkModificationUuids, activated);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/groups/{groupUuid}/duplications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Duplicate all modifications in a group and append them at the end of another modifications group")
    @ApiResponse(responseCode = "200", description = "The modifications have been duplicated")
    public ResponseEntity<Optional<NetworkModificationResult>> duplicateModificationsInGroup(@Parameter(description = "updated group UUID, where modifications are pasted") @PathVariable("groupUuid") UUID targetGroupUuid,
                                                                                             @Parameter(description = "the network uuid", required = true) @RequestParam(value = "networkUuid") UUID networkUuid,
                                                                                             @Parameter(description = "the report uuid", required = true) @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                                             @Parameter(description = "the reporter id", required = true) @RequestParam(value = "reporterId") UUID reporterId,
                                                                                             @Parameter(description = "the variant id", required = true) @RequestParam(value = "variantId") String variantId,
                                                                                             @Parameter(description = "origin group UUID, from where modifications are copied") @RequestParam(value = "duplicateFrom") UUID originGroupUuid) {
        return ResponseEntity.ok().body(networkModificationService.duplicateModificationsInGroup(targetGroupUuid, networkUuid, variantId, new ReportInfos(reportUuid, reporterId.toString()), originGroupUuid));
    }

    @DeleteMapping(value = "/groups/{groupUuid}/stashed-modifications")
    @Operation(summary = "Delete the stashed modifications in a group")
    @ApiResponse(responseCode = "200", description = "Stashed modifications in the group deleted")
    public ResponseEntity<Void> deleteStashedModificationInGroup(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                        @Parameter(description = "Return 404 if group is not found") @RequestParam(name = "errorOnGroupNotFound", required = false, defaultValue = "true") Boolean errorOnGroupNotFound) {
        networkModificationService.deleteStashedModificationInGroup(groupUuid, errorOnGroupNotFound);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/network-modifications/metadata", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get modifications metadata")
    @ApiResponse(responseCode = "200", description = "List of metadata used to describe modification elements")
    public ResponseEntity<List<ModificationMetadata>> getModificationsMetadata(@RequestParam("ids") List<UUID> ids) {
        return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON).body(networkModificationService.getModificationsMetadata(ids));
    }

    @PutMapping(value = "/networks/{networkUuid}/apply", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Apply a list of composite network modifications uuids passed in body")
    @ApiResponse(responseCode = "200", description = "The composite modifications have been applied")
    public ResponseEntity<Optional<NetworkModificationResult>> applyModifications(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                                  @Parameter(description = "the variant id", required = true) @RequestParam(value = "variantId") String variantId,
                                                                                  @Parameter(description = "the report uuid") @RequestParam(value = "reportUuid", required = false) UUID reportUuid,
                                                                                  @Parameter(description = "the reporter id") @RequestParam(value = "reporterId", required = false) String reporterId,
                                                                                  @RequestBody List<UUID> modificationsUuidList) {
        return ResponseEntity.ok().body(networkModificationService.applyModificationsFromUuids(networkUuid, variantId, new ReportInfos(reportUuid, reporterId), modificationsUuidList));
    }
}
