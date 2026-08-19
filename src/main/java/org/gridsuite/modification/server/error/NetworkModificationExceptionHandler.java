/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.error;

import com.powsybl.ws.commons.error.AbstractBusinessExceptionHandler;
import com.powsybl.ws.commons.error.PowsyblWsProblemDetail;
import com.powsybl.ws.commons.error.ServerNameProvider;
import jakarta.servlet.http.HttpServletRequest;
import lombok.NonNull;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@ControllerAdvice
public class NetworkModificationExceptionHandler extends AbstractBusinessExceptionHandler<NetworkModificationServerException, ModificationBusinessErrorCode> {

    protected NetworkModificationExceptionHandler(ServerNameProvider serverNameProvider) {
        super(serverNameProvider);
    }

    @Override
    protected @NonNull ModificationBusinessErrorCode getBusinessCode(NetworkModificationServerException e) {
        return e.getBusinessErrorCode();
    }

    @Override
    protected HttpStatus mapStatus(ModificationBusinessErrorCode modificationBusinessErrorCode) {
        return switch (modificationBusinessErrorCode) {
            case MODIFICATION_CONTAINER_NOT_FOUND,
                 MODIFICATION_CONTAINER_TYPE_NOT_FOUND,
                 MODIFICATION_NOT_FOUND,
                 MODIFICATIONS_NOT_FOUND,
                 NETWORK_NOT_FOUND,
                 VARIANT_NOT_FOUND
                 -> HttpStatus.NOT_FOUND;
            case MODIFICATION_CONTAINER_BAD_TYPE,
                 MODIFICATION_INFOS_ERROR,
                 MODIFICATION_WITH_GROUP_DELETION_FORBIDDEN,
                 MODIFICATION_DELETION_ARGUMENT_ERROR,
                 MODIFICATION_DUPLICATION_ARGUMENT_ERROR,
                 MODIFICATION_DESCRIPTION_MISSING,
                 MOVE_COMPOSITE_MODIFICATION_CYCLE_ERROR,
                 VOLTAGE_LEVEL_ATTACHMENT_LINE_MISSING
                -> HttpStatus.BAD_REQUEST;
            default -> HttpStatus.INTERNAL_SERVER_ERROR;
        };
    }

    @ExceptionHandler(NetworkModificationServerException.class)
    protected ResponseEntity<PowsyblWsProblemDetail> handleNetworkModificationException(
        NetworkModificationServerException exception, HttpServletRequest request) {
        return super.handleDomainException(exception, request);
    }
}
