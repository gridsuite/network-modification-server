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
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.CompositeInfos;
import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.springframework.data.util.Pair;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/network-composite-modifications")
@Tag(name = "network-modification-server - Composite modifications")
public class CompositeController {

    public enum CompositeModificationAction {
        SPLIT, // the network modifications contained into the composite modifications are extracted and inserted one by one
        INSERT // the composite modifications are fully inserted as composite modifications
    }

    private final NetworkModificationService networkModificationService;

    public CompositeController(NetworkModificationService networkModificationService) {
        this.networkModificationService = networkModificationService;
    }

    @PutMapping(value = "/groups/{groupUuid}", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Insert a list of composite network modifications passed in body at the end of a group")
    @ApiResponse(responseCode = "200", description = "The composite modification list has been added to the group.")
    public CompletableFuture<ResponseEntity<NetworkModificationsResult>> insertCompositeModifications(
            @Parameter(description = "updated group UUID, where modifications are inserted") @PathVariable("groupUuid") UUID targetGroupUuid,
            @Parameter(description = "Insertion method", required = true) @RequestParam(value = "action") CompositeModificationAction action,
            @RequestBody Pair<List<CompositeInfos>, List<ModificationApplicationContext>> modificationContextInfos) {
        return switch (action) {
            case SPLIT -> networkModificationService.splitCompositeModifications(
                            targetGroupUuid,
                            modificationContextInfos
                    ).thenApply(ResponseEntity.ok()::body);
            case INSERT -> networkModificationService.insertCompositeModifications(
                            targetGroupUuid,
                            modificationContextInfos
                    ).thenApply(ResponseEntity.ok()::body);
        };
    }

    @PostMapping(value = "/", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Assemble some network modifications into a new composite modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The composite modification has been created")})
    public ResponseEntity<UUID> assembleNetworkModificationsIntoNewComposite(
            @RequestBody List<UUID> assembledModificationsUuids) {
        return ResponseEntity.ok().body(
                networkModificationService.assembleNetworkModificationsIntoNewComposite(assembledModificationsUuids)
        );
    }

    @PostMapping(value = "", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create a network composite modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The composite modification has been created")})
    public ResponseEntity<UUID> createNetworkCompositeModification(@Parameter(description = "Composite modifications name", required = true) @RequestParam("name") String name,
                                                                   @RequestBody List<UUID> modificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.createNetworkCompositeModification(modificationUuids, name));
    }

    @GetMapping(value = "/network-modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get the list of all the network modifications inside a list of composite modifications")
    @ApiResponse(responseCode = "200", description = "Map of modifications inside the composite modifications for each composite")
    public ResponseEntity<Map<UUID,
            List<ModificationInfos>>> getNetworkModificationsFromComposite(@Parameter(description = "Composite modifications uuids list") @RequestParam("uuids") List<UUID> compositeModificationUuids,
                                                                                        @Parameter(description = "Only metadata") @RequestParam(name = "onlyMetadata", required = false,
                                                                                                defaultValue = "true") Boolean onlyMetadata) {
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(networkModificationService.getNetworkModificationsFromComposite(compositeModificationUuids, onlyMetadata)
                );
    }

    @GetMapping(value = "/children-uuids", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Recursively expand a list of modification UUIDs with composites and their content UUIDs")
    @ApiResponse(responseCode = "200", description = "The full set of leaf modification UUIDs")
    public ResponseEntity<List<UUID>> findAllChildrenUuids(@Parameter(description = "Modification UUIDs to expand") @RequestParam("uuids") List<UUID> compositesModificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.findAllChildrenUuids(compositesModificationUuids));
    }

    @PostMapping(value = "/duplication", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Duplicate some composite modifications")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The duplicated modifications uuids mapped with their source uuid")})
    public ResponseEntity<Map<UUID, UUID>> duplicateCompositeModifications(@Parameter(description = "source modifications uuids list to duplicate") @RequestBody List<UUID> sourceModificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.duplicateCompositeModifications(sourceModificationUuids));
    }

    @PutMapping(value = "/{uuid}", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Update a network composite modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The composite modification has been updated")})
    public ResponseEntity<Void> updateNetworkCompositeModification(
            @PathVariable("uuid") UUID compositeModificationUuid,
            @Parameter(description = "New composite name") @RequestParam(value = "name", required = false) String name) {
        networkModificationService.updateCompositeModification(compositeModificationUuid, name);
        return ResponseEntity.ok().build();
    }

    /**
     * @return modification uuid -> uuid of the composite currently containing it; modifications sitting directly
     * under a group (or not found) have no entry, letting the caller resolve the ambiguous case (unlike
     * network-modification-server's /network-modifications/references, this works for any modification, not just references)
     */
    @GetMapping(value = "/parent-composites", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "For each given network modification, find the composite currently containing it")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The parent composites were returned")})
    public ResponseEntity<Map<UUID, UUID>> getParentComposites(
            @Parameter(description = "Network modification UUIDs") @RequestParam("uuids") List<UUID> networkModificationUuids) {
        return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
                .body(networkModificationService.findModificationParentComposites(networkModificationUuids));
    }

    /**
     * @return modification uuid -> uuid of the top-level group ultimately containing it, walking up through as
     * many nested composite modifications as needed; modifications not reachable from any group have no entry
     */
    @GetMapping(value = "/root-groups", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "For each given network modification, find the top-level group ultimately containing it")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The root groups were returned")})
    public ResponseEntity<Map<UUID, UUID>> getRootGroups(
            @Parameter(description = "Network modification UUIDs") @RequestParam("uuids") List<UUID> networkModificationUuids) {
        return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
                .body(networkModificationService.findModificationRootGroups(networkModificationUuids));
    }

    @PutMapping(value = "/{uuid}/replace", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Replaces all the network modifications inside a network composite modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The composite modification has been updated")})
    public ResponseEntity<Void> replaceNetworkCompositeModification(@PathVariable("uuid") UUID compositeModificationUuid,
                                                                    @Parameter(description = "New composite name") @RequestParam(value = "name") String name,
                                                                    @RequestBody List<UUID> modificationUuids) {
        networkModificationService.replaceCompositeModification(compositeModificationUuid, name, modificationUuids);
        return ResponseEntity.ok().build();
    }
}
