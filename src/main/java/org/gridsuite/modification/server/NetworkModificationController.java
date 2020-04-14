/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import io.swagger.annotations.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RestController
@RequestMapping(value = "/" + NetworkModificationApi.API_VERSION + "/")
@Api(tags = "network-modification-server")
@ComponentScan(basePackageClasses = NetworkModificationService.class)
public class NetworkModificationController {

    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationController.class);

    @Inject
    private NetworkModificationService networkModificationService;

    @PutMapping(value = "/networks/{networkUuid}/switches/{switchId}")
    @ApiOperation(value = "change a switch state in the network")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The switch state has been changed")})
    public ResponseEntity<Void> changeSwitchState(
            @ApiParam(value = "Network UUID") @PathVariable("networkUuid") UUID networkUuid,
            @ApiParam(value = "Switch ID") @PathVariable("switchId") String switchId,
            @RequestParam("open") String open) {
        networkModificationService.changeSwitchState(networkUuid, switchId, open);
        return ResponseEntity.ok().build();
    }
}
