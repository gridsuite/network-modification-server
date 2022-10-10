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
    public ResponseEntity<List<EquipmentModificationInfos>> changeSwitchState(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
            @Parameter(description = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam(value = "group", required = false) UUID groupUuid,
            @RequestParam(value = "reportUuid") UUID reportUuid,
            @RequestParam("open") String open) {
        return ResponseEntity.ok().body(networkModificationService.changeSwitchState(networkUuid, variantId, groupUuid, reportUuid, switchId, Boolean.parseBoolean(open)));
    }

    @PutMapping(value = "/networks/{networkUuid}/groovy", consumes = MediaType.TEXT_PLAIN_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "change an equipment state in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The equipment state has been changed"),
                           @ApiResponse(responseCode = "404", description = "the network or equipment not found")})
    public ResponseEntity<List<ModificationInfos>> applyGroovyScript(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                     @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                     @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                     @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                     @RequestBody String groovyScript) {
        return ResponseEntity.ok().body(networkModificationService.applyGroovyScript(networkUuid, variantId, groupUuid, reportUuid, groovyScript));
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
                                                  @RequestParam("duplicateFrom") UUID sourceGroupUuid,
                                                  @RequestParam("reportUuid") UUID reportUuid) {
        networkModificationService.createModificationGroup(sourceGroupUuid, groupUuid, reportUuid);
        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/modifications/{modificationUuid}", produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "Get a modification")
    @ApiResponse(responseCode = "200", description = "The modification")
    public ResponseEntity<List<ModificationInfos>> getModification(@Parameter(description = "Modification UUID") @PathVariable("modificationUuid") UUID modificationUuid) {
        return ResponseEntity.ok().body(networkModificationService.getModification(modificationUuid));
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

    @DeleteMapping(value = "/groups/{groupUuid}/modifications")
    @Operation(summary = "Delete modifications from a group")
    @ApiResponse(responseCode = "200", description = "Modifications deleted")
    public ResponseEntity<Void> deleteModifications(@Parameter(description = "Group UUID") @PathVariable("groupUuid") UUID groupUuid,
                                                          @Parameter(description = "modifications to delete", required = true) @RequestParam(value = "modificationsUuids") Set<UUID> modificationsUuids) {
        networkModificationService.deleteModifications(groupUuid, modificationsUuids);
        return ResponseEntity.ok().build();
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

    @PutMapping(value = "/networks/{networkUuid}/lines/{lineId}/status", consumes = MediaType.TEXT_PLAIN_VALUE)
    @Operation(summary = "Change the status of a line in a network variant")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "The line status has been changed"),
        @ApiResponse(responseCode = "404", description = "The network or line is not found"),
        @ApiResponse(responseCode = "400", description = "The modification action is incorrect")
    })
    public ResponseEntity<List<ModificationInfos>> changeLineStatus(
            @Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
            @Parameter(description = "Line ID") @PathVariable("lineId") String lineId,
            @RequestParam(value = "group", required = false) UUID groupUuid,
            @RequestParam(value = "reportUuid") UUID reportUuid,
            @RequestBody(required = false) String action) {
        return ResponseEntity.ok().body(networkModificationService.changeLineStatus(networkUuid, variantId, groupUuid, reportUuid, lineId, action));
    }

    @PostMapping(value = "/networks/{networkUuid}/loads", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a load in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The load has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createLoad(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                       @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                       @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                       @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                       @RequestBody LoadCreationInfos loadCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createLoad(networkUuid, variantId, groupUuid, reportUuid, loadCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/loads-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a load creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The load creation has been updated")})
    public ResponseEntity<Void> updateLoadCreation(@PathVariable("modificationUuid") UUID modificationUuid,
                                                         @RequestBody LoadCreationInfos loadCreationInfos) {
        networkModificationService.updateLoadCreation(loadCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/modifications/{modificationUuid}/loads-modification", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a load modification in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The load modification has been updated")})
    public ResponseEntity<Void> updateLoadModification(@PathVariable("modificationUuid") UUID modificationUuid,
                                                         @RequestBody LoadModificationInfos loadModificationInfos) {
        networkModificationService.updateLoadModification(loadModificationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/networks/{networkUuid}/loads-modification", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "modify a load in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The load has been modified")})
    public ResponseEntity<List<EquipmentModificationInfos>> modifyLoad(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                       @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                       @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                       @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                       @RequestBody LoadModificationInfos loadModificationInfos) {
        return ResponseEntity.ok().body(networkModificationService.modifyLoad(networkUuid, variantId, groupUuid, reportUuid, loadModificationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/generators-modification", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "modify a generator modification in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The generator modification  has been modified")})
    public ResponseEntity<Void> modifyGeneratorModification(@Parameter(description = "Modification UUID") @PathVariable("modificationUuid") UUID modificationUuid,
                                                                       @RequestBody GeneratorModificationInfos generatorModificationInfos) {
        networkModificationService.updateModifyGenerator(modificationUuid, generatorModificationInfos);
        return ResponseEntity.ok().build();
    }

    @PutMapping(value = "/networks/{networkUuid}/generators-modification", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "modify a generator in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The generator has been modified")})
    public ResponseEntity<List<EquipmentModificationInfos>> modifyGenerator(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                            @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                            @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                            @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                            @RequestBody GeneratorModificationInfos generatorModificationInfos) {
        return ResponseEntity.ok().body(networkModificationService.modifyGenerator(networkUuid, variantId, groupUuid, reportUuid, generatorModificationInfos));
    }

    @PostMapping(value = "/networks/{networkUuid}/generators", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a generator in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The generator has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createGenerator(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                            @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                            @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                            @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                            @RequestBody GeneratorCreationInfos generatorCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createGenerator(networkUuid, variantId, groupUuid, reportUuid, generatorCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/generators-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a generator creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The generator creation has been updated")})
    public ResponseEntity<Void> updateGeneratorCreation(@PathVariable("modificationUuid") UUID modificationUuid,
                                                              @RequestBody GeneratorCreationInfos generatorCreationInfos) {
        networkModificationService.updateGeneratorCreation(generatorCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/shunt-compensators", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a shunt compensator in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The shunt compensator has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createShuntCompensator(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                                   @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                                   @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                                   @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                                   @RequestBody ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createShuntCompensator(networkUuid, variantId, groupUuid, reportUuid, shuntCompensatorCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/shunt-compensators-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a shunt-compensator creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The shunt compensator creation has been updated")})
    public ResponseEntity<Void> updateShuntCompensatorCreation(@PathVariable("modificationUuid") UUID modificationUuid,
                                                                     @RequestBody ShuntCompensatorCreationInfos shuntCompensatorCreationInfos) {
        networkModificationService.updateShuntCompensatorCreation(shuntCompensatorCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/lines", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a line in a network variant")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createLine(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                       @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                       @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                       @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                       @RequestBody LineCreationInfos lineCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createLine(networkUuid, variantId, groupUuid, reportUuid, lineCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/lines-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a line creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line creation has been updated")})
    public ResponseEntity<Void> updateLineCreation(@PathVariable("modificationUuid") UUID modificationUuid,
                                                         @RequestBody LineCreationInfos lineCreationInfos) {
        networkModificationService.updateLineCreation(lineCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/two-windings-transformers", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a two windings transformer in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The two windings transformer has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createTwoWindingsTransformer(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                                         @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                                         @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                                         @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                                         @RequestBody TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createTwoWindingsTransformer(networkUuid, variantId, groupUuid, reportUuid, twoWindingsTransformerCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/two-windings-transformers-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a two windings transformer creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The two windings transformer creation has been updated")})
    public ResponseEntity<Void> updateTwoWindingsTransformer(@PathVariable("modificationUuid") UUID modificationUuid,
                                                                   @RequestBody TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        networkModificationService.updateTwoWindingsTransformerCreation(twoWindingsTransformerCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/substations", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a substation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The substation has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createSubstation(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                             @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                             @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                             @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                             @RequestBody SubstationCreationInfos substationCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createSubstation(networkUuid, variantId, groupUuid, reportUuid, substationCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/substations-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a substation creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The substation creation has been updated")})
    public ResponseEntity<Void> updateSubstationCreation(@PathVariable("modificationUuid") UUID modificationUuid,
                                                               @RequestBody SubstationCreationInfos substationCreationInfos) {
        networkModificationService.updateSubstationCreation(substationCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/voltage-levels", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "create a voltage level in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The voltage level has been created")})
    public ResponseEntity<List<EquipmentModificationInfos>> createVoltageLevel(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                               @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                               @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                               @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                               @RequestBody VoltageLevelCreationInfos voltageLevelCreationInfos) {
        return ResponseEntity.ok().body(networkModificationService.createVoltageLevel(networkUuid, variantId, groupUuid, reportUuid, voltageLevelCreationInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/voltage-levels-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a voltage level creation in the network")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The voltage level creation has been updated")})
    public ResponseEntity<Void> updateVoltageLevelCreation(@PathVariable("modificationUuid") UUID modificationUuid,
                                                                 @RequestBody VoltageLevelCreationInfos voltageLevelCreationInfos) {
        networkModificationService.updateVoltageLevelCreation(voltageLevelCreationInfos, modificationUuid);
        return ResponseEntity.ok().build();
    }

    @DeleteMapping(value = "/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}")
    @Operation(summary = "Delete an equipment in a network variant")
    @ApiResponse(responseCode = "200", description = "The equipment has been deleted")
    public ResponseEntity<List<EquipmentDeletionInfos>> deleteEquipment(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                        @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                        @Parameter(description = "Equipment type") @PathVariable("equipmentType") String equipmentType,
                                                                        @Parameter(description = "Equipment id") @PathVariable("equipmentId") String equipmentId,
                                                                        @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                        @RequestParam(value = "reportUuid") UUID reportUuid) {
        return ResponseEntity.ok().body(networkModificationService.deleteEquipment(networkUuid, variantId, groupUuid, reportUuid, equipmentType, equipmentId));
    }

    @PostMapping(value = "/networks/{networkUuid}/line-splits", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "split a line at a voltage level")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line has been split")})
    public ResponseEntity<List<ModificationInfos>> lineSplitWithVoltageLevel(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                             @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                             @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                             @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                             @RequestBody LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        return ResponseEntity.ok().body(networkModificationService.splitLineWithVoltageLevel(networkUuid, variantId, groupUuid, reportUuid, lineSplitWithVoltageLevelInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/line-splits", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update a line split")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line split at voltage level has been updated")})
    public ResponseEntity<Void> updateLineSplitWithVoltageLevel(@PathVariable("modificationUuid") UUID modificationUuid,
        @RequestBody LineSplitWithVoltageLevelInfos lineSplitWithVoltageLevelInfos) {
        networkModificationService.updateLineSplitWithVoltageLevel(modificationUuid, lineSplitWithVoltageLevelInfos);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/line-attach", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "attach line to a voltage level")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line has been attached to voltage level")})
    public ResponseEntity<List<ModificationInfos>> lineAttachToVoltageLevel(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                            @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                            @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                            @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                            @RequestBody LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        return ResponseEntity.ok().body(networkModificationService.createLineAttachToVoltageLevel(networkUuid, variantId, groupUuid, reportUuid, lineAttachToVoltageLevelInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/line-attach-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update line attachment")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The line attachment to voltage level has been updated")})
    public ResponseEntity<Void> updateLineAttachToVoltageLevel(@PathVariable("modificationUuid") UUID modificationUuid,
                                                                     @RequestBody LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        networkModificationService.updateLineAttachToVoltageLevel(modificationUuid, lineAttachToVoltageLevelInfos);
        return ResponseEntity.ok().build();
    }

    @PostMapping(value = "/networks/{networkUuid}/lines-attach-to-split-lines", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "attach lines to a split lines")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The lines has been attached to split lines")})
    public ResponseEntity<List<ModificationInfos>> linesAttachToSplitLines(@Parameter(description = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                                         @Parameter(description = "Variant Id") @RequestParam(name = "variantId", required = false) String variantId,
                                                                         @RequestParam(value = "group", required = false) UUID groupUuid,
                                                                         @RequestParam(value = "reportUuid") UUID reportUuid,
                                                                         @RequestBody LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        return ResponseEntity.ok().body(networkModificationService.createLinesAttachToSplitLines(networkUuid, variantId, groupUuid, reportUuid, linesAttachToSplitLinesInfos));
    }

    @PutMapping(value = "/modifications/{modificationUuid}/lines-attach-to-split-lines-creation", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
    @Operation(summary = "update attach lines to split lines")
    @ApiResponses(value = {@ApiResponse(responseCode = "200", description = "The lines attachment to split lines has been updated")})
    public ResponseEntity<Void> updateLinesAttachToSplitLines(@PathVariable("modificationUuid") UUID modificationUuid,
                                                            @RequestBody LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        networkModificationService.updateLinesAttachToSplitLines(modificationUuid, linesAttachToSplitLinesInfos);
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
