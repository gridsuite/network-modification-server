/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.error;

import com.powsybl.ws.commons.error.ErrorUtils;
import com.powsybl.ws.commons.error.PowsyblWsProblemDetail;
import com.powsybl.ws.commons.error.ServerNameProvider;
import jakarta.servlet.http.HttpServletRequest;
import org.gridsuite.modification.error.NetworkModificationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@ControllerAdvice
public class NetworkModificationExceptionHandler {

    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationExceptionHandler.class);

    private final ServerNameProvider serverNameProvider;

    protected NetworkModificationExceptionHandler(ServerNameProvider serverNameProvider) {
        this.serverNameProvider = serverNameProvider;
    }

    @ExceptionHandler(NetworkModificationException.class)
    protected ResponseEntity<PowsyblWsProblemDetail> handleNetworkModificationException(NetworkModificationException exception, HttpServletRequest request) {
        LOGGER.warn(exception.getMessage(), exception);
        HttpStatus status = HttpStatus.INTERNAL_SERVER_ERROR; // basic handling, we do not want specific http error code for each exception

        PowsyblWsProblemDetail problemDetail = ErrorUtils.baseBuilder(serverNameProvider.serverName(), status, request)
                .detail(exception.getMessage())
                .build();
        return ResponseEntity.status(status).body(problemDetail);
    }
}
