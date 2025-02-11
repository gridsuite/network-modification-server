/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import org.gridsuite.modification.NetworkModificationException;
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
public class RestResponseEntityExceptionHandler {

    private static final Logger LOGGER = LoggerFactory.getLogger(RestResponseEntityExceptionHandler.class);
    private static final String HANDLER_MESSAGE = "Caught in handler";

    @ExceptionHandler(NetworkModificationException.class)
    protected ResponseEntity<Object> handleException(NetworkModificationException exception) {
        return handleException(exception.getType().status, exception);
    }

    @ExceptionHandler(NetworkModificationServerException.class)
    protected ResponseEntity<Object> handleException(NetworkModificationServerException exception) {
        return handleException(exception.getType().status, exception);
    }

    private ResponseEntity<Object> handleException(HttpStatus status, Exception exception) {
        if (LOGGER.isErrorEnabled()) {
            LOGGER.error(HANDLER_MESSAGE, exception);
        }
        return ResponseEntity
            .status(status)
            .body(exception.getMessage());
    }
}
