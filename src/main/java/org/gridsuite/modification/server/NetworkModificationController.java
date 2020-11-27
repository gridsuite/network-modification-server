/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import io.swagger.annotations.*;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.List;
import java.util.Set;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/")
@Api(tags = "network-modification-server")
@ComponentScan(basePackageClasses = NetworkModificationService.class)
public class NetworkModificationController {

    @Inject
    private NetworkModificationService networkModificationService;

    @PutMapping(value = "/networks/{networkUuid}/switches/{switchId}", produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "change a switch state in the network", response = List.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The switch state has been changed")})
    public ResponseEntity<Set<String>> changeSwitchState(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam("open") String open) {
        return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON).body(networkModificationService.changeSwitchState(networkUuid, switchId, open));
    }

    @PutMapping(value = "/networks/{networkUuid}/groovy/", produces = MediaType.APPLICATION_JSON_VALUE)
    @ApiOperation(value = "change an equipment state in the network", response = Set.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The equipment state has been changed")})
    public ResponseEntity<Set<String>> applyGroovyScript(@ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
                                                         @RequestBody String groovyScript) {
        Pair<Boolean, Set<String>> modifications = networkModificationService.applyGroovyScript(networkUuid, groovyScript);
        if (modifications.getLeft()) {
            return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON).body(modifications.getRight());
        }
        return ResponseEntity.badRequest().build();
    }
}
