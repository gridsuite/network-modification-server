/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.OperationalStatusModificationInfos;
import org.springframework.http.HttpStatus;

import java.util.Objects;

import static org.gridsuite.modification.server.NetworkModificationException.Type.ATTRIBUTE_NOT_EDITABLE;

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
        NOTHING_TO_DELETE(HttpStatus.BAD_REQUEST),
        MODIFICATION_GROUP_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFICATION_NOT_FOUND(HttpStatus.NOT_FOUND),
        SWITCH_NOT_FOUND(HttpStatus.NOT_FOUND),
        LINE_NOT_FOUND(HttpStatus.NOT_FOUND),
        BRANCH_NOT_FOUND(HttpStatus.NOT_FOUND),
        LOAD_NOT_FOUND(HttpStatus.NOT_FOUND),
        BATTERY_NOT_FOUND(HttpStatus.NOT_FOUND),
        GENERATOR_NOT_FOUND(HttpStatus.NOT_FOUND),
        TWO_WINDINGS_TRANSFORMER_NOT_FOUND(HttpStatus.NOT_FOUND),
        UNKNOWN_MODIFICATION_TYPE(HttpStatus.INTERNAL_SERVER_ERROR),
        UNKNOWN_EQUIPMENT_TYPE(HttpStatus.INTERNAL_SERVER_ERROR),
        WRONG_EQUIPMENT_TYPE(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        VOLTAGE_LEVEL_NOT_FOUND(HttpStatus.NOT_FOUND),
        CREATE_LOAD_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_LOAD_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        BUSBAR_SECTION_NOT_FOUND(HttpStatus.NOT_FOUND),
        BUS_NOT_FOUND(HttpStatus.NOT_FOUND),
        CREATE_BATTERY_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_GENERATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_SHUNT_COMPENSATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_SHUNT_COMPENSATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        DELETE_EQUIPMENT_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        EQUIPMENT_NOT_FOUND(HttpStatus.NOT_FOUND),
        ATTRIBUTE_NOT_EDITABLE(HttpStatus.BAD_REQUEST),
        CREATE_LINE_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_LINE_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_TWO_WINDINGS_TRANSFORMER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_SUBSTATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_SUBSTATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_VOLTAGE_LEVEL_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        MODIFY_VOLTAGE_LEVEL_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        SUBSTATION_NOT_FOUND(HttpStatus.NOT_FOUND),
        BATTERY_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        LOAD_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        VOLTAGE_LEVEL_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        BUSBAR_SECTION_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        BUSBAR_SECTION_NOT_DEFINED(HttpStatus.BAD_REQUEST),
        GENERATOR_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        SHUNT_COMPENSATOR_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        SHUNT_COMPENSATOR_NOT_FOUND(HttpStatus.NOT_FOUND),
        LINE_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        TWO_WINDINGS_TRANSFORMER_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        TWO_WINDINGS_TRANSFORMER_CREATION_ERROR(HttpStatus.BAD_REQUEST),
        CONNECTION_POSITION_ERROR(HttpStatus.BAD_REQUEST),
        MODIFY_BATTERY_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        OPERATIONAL_EQUIPMENT_ACTION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        OPERATIONAL_EQUIPMENT_ACTION_TYPE_EMPTY(HttpStatus.BAD_REQUEST, "Empty equipment action type"),
        OPERATIONAL_EQUIPMENT_ACTION_TYPE_UNKNOWN(HttpStatus.BAD_REQUEST),
        OPERATIONAL_EQUIPMENT_ACTION_TYPE_UNSUPPORTED(HttpStatus.INTERNAL_SERVER_ERROR),
        LINE_SPLIT_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        LINE_SPLIT_NOT_FOUND(HttpStatus.NOT_FOUND),
        LINE_ATTACH_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        LINE_ATTACH_DESCRIPTION_ERROR(HttpStatus.BAD_REQUEST),
        LINE_ATTACH_NOT_FOUND(HttpStatus.NOT_FOUND),
        MODIFY_GENERATOR_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        TYPE_MISMATCH(HttpStatus.BAD_REQUEST),
        MISSING_MODIFICATION_DESCRIPTION(HttpStatus.BAD_REQUEST),
        MODIFICATION_OUT_OF_RANGE(HttpStatus.BAD_REQUEST),
        POSITION_ORDER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        DELETE_VOLTAGE_LEVEL_ON_LINE_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        DELETE_VOLTAGE_LEVEL_ON_LINE_NOT_FOUND(HttpStatus.NOT_FOUND),
        EQUIPMENT_ATTRIBUTE_NAME_ERROR(HttpStatus.BAD_REQUEST),
        EQUIPMENT_ATTRIBUTE_VALUE_ERROR(HttpStatus.BAD_REQUEST),
        MOVE_MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        GENERATOR_SCALING_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        LOAD_SCALING_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        DELETE_ATTACHING_LINE_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        DELETE_ATTACHING_LINE_NOT_FOUND(HttpStatus.NOT_FOUND),
        FILTERS_NOT_FOUND(HttpStatus.NOT_FOUND),
        GENERATION_DISPATCH_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        PRELOADING_STRATEGY_NOT_ALLOWED(HttpStatus.INTERNAL_SERVER_ERROR),
        VOLTAGE_INIT_MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        TABULAR_MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        CREATE_VSC_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        HVDC_LINE_ALREADY_EXISTS(HttpStatus.BAD_REQUEST),
        VSC_CONVERTER_STATION_NOT_FOUND(HttpStatus.NOT_FOUND),
        CREATE_CONVERTER_STATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR),
        BY_FORMULA_MODIFICATION_ERROR(HttpStatus.INTERNAL_SERVER_ERROR);

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

    public Type getType() {
        return type;
    }

    public static NetworkModificationException createEquipmentTypeUnknown(@NonNull String type) {
        return new NetworkModificationException(Type.UNKNOWN_EQUIPMENT_TYPE, "The equipment type : " + type + " is unknown");
    }

    public static NetworkModificationException createBranchActionTypeUnsupported(@NonNull OperationalStatusModificationInfos.ActionType type) {
        return new NetworkModificationException(Type.OPERATIONAL_EQUIPMENT_ACTION_TYPE_UNSUPPORTED, "The branch action type : " + type + " is unsupported");
    }

    public static NetworkModificationException createEquipementAttributeNotEditable(@NonNull IdentifiableType equipmentType, @NonNull String attributeName) {
        throw new NetworkModificationException(ATTRIBUTE_NOT_EDITABLE, equipmentType.name() + " attribute '" + attributeName + "' not editable");
    }
}
