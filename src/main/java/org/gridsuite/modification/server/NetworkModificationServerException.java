/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import lombok.Getter;
import org.springframework.http.HttpStatus;

import java.util.Objects;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkModificationServerException extends PowsyblException {
    public enum Type {
        DUPLICATION_ARGUMENT_INVALID(HttpStatus.BAD_REQUEST, "Invalid argument for duplication");

        public final HttpStatus status;
        private final String message;

        Type(HttpStatus status, String message) {
            this.status = status;
            this.message = message;
        }
    }

    @Getter
    private final Type type;

    public NetworkModificationServerException(Type type) {
        super(Objects.requireNonNull(type.name()) + ((type.message == null) ? "" : " : " + type.message));
        this.type = type;
    }
}
