/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@ControllerAdvice
public class RestResponseEntityExceptionHandler {

    @ExceptionHandler(value = {NetworkModificationException.class})
    protected ResponseEntity<Object> handleException(RuntimeException exception) {
        NetworkModificationException networkModificationException = (NetworkModificationException) exception;
        return ResponseEntity
                .status(networkModificationException.getType().getStatus())
                .body(networkModificationException.getMessage());
    }
}
