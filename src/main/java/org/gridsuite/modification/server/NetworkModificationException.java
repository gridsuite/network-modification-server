/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import org.springframework.http.HttpStatus;

import java.util.Objects;


/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class NetworkModificationException extends RuntimeException {
    static final String EMPTY_SCRIPT = "Empty script";

    public enum Type {
        GROOVY_SCRIPT_EMPTY(HttpStatus.BAD_REQUEST, EMPTY_SCRIPT),
        GROOVY_SCRIPT_ERROR(HttpStatus.BAD_REQUEST),
        NETWORK_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFICATION_GROUP_NOT_FOUND(HttpStatus.NOT_FOUND),
        SWITCH_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR);

        public final HttpStatus status;
        private final String message;

        HttpStatus getStatus() {
            return status;
        }

        Type(HttpStatus status) {
            this(status, null);
        }

        Type(HttpStatus status, String message) {
            this.status = status;
            this.message = message;
        }
    }

    private final Type type;

    public NetworkModificationException(Type type) {
        super(Objects.requireNonNull(type.name()) + ((type.message == null) ? "" : " : " + type.message));
        this.type = type;
    }

    public NetworkModificationException(Type type, Exception cause) {
        super(Objects.requireNonNull(type.name()) + " : " + Objects.requireNonNull(cause.getMessage()), cause);
        this.type = type;
    }

    public NetworkModificationException(Type type, String message) {
        super(Objects.requireNonNull(type.name()) + " : " + Objects.requireNonNull(message));
        this.type = type;
    }

    Type getType() {
        return type;
    }
}
