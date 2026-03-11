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
import org.gridsuite.modification.dto.ModificationCompositeInfos;
import org.gridsuite.modification.dto.ModificationInfos;
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
    @Operation(summary = "For a list of composite network modifications passed in body, insert them at the end of the list as complete composite modifications or split into their network modification content ")
    @ApiResponse(responseCode = "200", description = "The composite modification list has been added to the group.")
    public CompletableFuture<ResponseEntity<NetworkModificationsResult>> insertCompositeModifications(
            @Parameter(description = "updated group UUID, where modifications are pasted") @PathVariable("groupUuid") UUID targetGroupUuid,
            @Parameter(description = "kind of modification", required = true) @RequestParam(value = "action") CompositeModificationAction action,
            @RequestBody Pair<List<ModificationCompositeInfos>, List<ModificationApplicationContext>> modificationContextInfos) {
        List<UUID> modificationsUuids = modificationContextInfos.getFirst().stream().map(ModificationCompositeInfos::getUuid).toList();
        return switch (action) {
            case SPLIT ->
                    networkModificationService.splitCompositeModifications(targetGroupUuid, modificationsUuids, modificationContextInfos.getSecond())
                            .thenApply(ResponseEntity.ok()::body);
            case INSERT ->
                    networkModificationService.insertCompositeModificationsIntoGroup(
                            targetGroupUuid,
                            modificationContextInfos.getFirst(),
                            modificationContextInfos.getSecond()
                    ).thenApply(ResponseEntity.ok()::body);
        };
    }

    @PostMapping(value = "", consumes = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Create a network composite modification")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The composite modification has been created")})
    public ResponseEntity<UUID> createNetworkCompositeModification(@RequestBody List<UUID> modificationUuids) {
        return ResponseEntity.ok().body(networkModificationService.createNetworkCompositeModification(modificationUuids));
    }

    @GetMapping(value = "/network-modifications", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get the list of all the network modifications inside a list of composite modifications")
    @ApiResponse(responseCode = "200", description = "List of modifications inside the composite modifications")
    public ResponseEntity<List<ModificationInfos>> getNetworkModificationsFromComposite(@Parameter(description = "Composite modifications uuids list") @RequestParam("uuids") List<UUID> compositeModificationUuids,
                                                                                        @Parameter(description = "Only metadata") @RequestParam(name = "onlyMetadata", required = false, defaultValue = "true") Boolean onlyMetadata) {
        return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .body(networkModificationService.getNetworkModificationsFromComposite(compositeModificationUuids, onlyMetadata)
                );
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
    public ResponseEntity<Void> updateNetworkCompositeModification(@PathVariable("uuid") UUID compositeModificationUuid,
                                                                   @RequestBody List<UUID> modificationUuids) {
        networkModificationService.updateCompositeModification(compositeModificationUuid, modificationUuids);
        return ResponseEntity.ok().build();
    }
}
