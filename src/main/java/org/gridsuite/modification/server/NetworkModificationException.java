/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.springframework.http.HttpStatus;

import java.util.Objects;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class NetworkModificationException extends PowsyblException {
    public enum Type {
        GROOVY_SCRIPT_EMPTY(HttpStatus.BAD_REQUEST, "Empty script"),
        GROOVY_SCRIPT_ERROR(HttpStatus.BAD_REQUEST),
        NETWORK_NOT_FOUND(HttpStatus.NOT_FOUND),
        VARIANT_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFICATION_GROUP_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFICATION_NOT_FOUND(HttpStatus.NOT_FOUND),
        SWITCH_NOT_FOUND(HttpStatus.NOT_FOUND),
        LINE_NOT_FOUND(HttpStatus.NOT_FOUND),
        LOAD_NOT_FOUND(HttpStatus.NOT_FOUND),
        UNKNOWN_EQUIPMENT_TYPE(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        VOLTAGE_LEVEL_NOT_FOUND(HttpStatus.NOT_FOUND),
        CREATE_LOAD_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_LOAD_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        BUSBAR_SECTION_NOT_FOUND(HttpStatus.NOT_FOUND),
        BUS_NOT_FOUND(HttpStatus.NOT_FOUND),
        CREATE_GENERATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_SHUNT_COMPENSATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        DELETE_EQUIPMENT_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        EQUIPMENT_NOT_FOUND(HttpStatus.NOT_FOUND),
        CREATE_LINE_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_TWO_WINDINGS_TRANSFORMER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_SUBSTATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_VOLTAGE_LEVEL_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        SUBSTATION_NOT_FOUND(HttpStatus.NOT_FOUND),
        BRANCH_ACTION_ERROR(HttpStatus.BAD_REQUEST),
        BRANCH_ACTION_TYPE_EMPTY(HttpStatus.BAD_REQUEST, "Empty branch action type"),
        BRANCH_ACTION_TYPE_UNKNOWN(HttpStatus.BAD_REQUEST),
        BRANCH_ACTION_TYPE_UNSUPPORTED(HttpStatus.INTERNAL_SERVER_ERROR),
        LINE_SPLIT_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        LINE_SPLIT_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFICATION_OUT_OF_RANGE(HttpStatus.BAD_REQUEST);

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
        super(Objects.requireNonNull(type.name()) + " : " + ((cause.getMessage() == null) ? cause.getClass().getName() : cause.getMessage()), cause);
        this.type = type;
    }

    public NetworkModificationException(Type type, String message) {
        super(Objects.requireNonNull(type.name()) + " : " + Objects.requireNonNull(message));
        this.type = type;
    }

    Type getType() {
        return type;
    }

    public static NetworkModificationException createEquipmentTypeUnknown(@NonNull String type) {
        return new NetworkModificationException(Type.UNKNOWN_EQUIPMENT_TYPE, "The equipment type : " + type + " is unknown");
    }

    public static NetworkModificationException createBranchActionTypeUnsupported(@NonNull BranchStatusModificationInfos.ActionType type) {
        return new NetworkModificationException(Type.BRANCH_ACTION_TYPE_UNSUPPORTED, "The branch action type : " + type + " is unsupported");
    }

    public static NetworkModificationException createBranchActionTypeUnknown(@NonNull String type) {
        return new NetworkModificationException(Type.BRANCH_ACTION_TYPE_UNKNOWN, "The branch action type : " + type + " is unknown");
    }
}
