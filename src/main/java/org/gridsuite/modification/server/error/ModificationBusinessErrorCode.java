/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.error;

import com.powsybl.ws.commons.error.BusinessErrorCode;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public enum ModificationBusinessErrorCode implements BusinessErrorCode {
    MODIFICATION_CONTAINER_NOT_FOUND("modification.container.notFound", "Modification container '%s' of type '%s' not found"),
    MODIFICATION_CONTAINER_TYPE_NOT_FOUND("modification.container.type.notFound", "Modification container type of %s is not found"),
    MODIFICATION_CONTAINER_BAD_TYPE("modification.container.badType", "Modification container type of %s is invalid : actual type %s -> expected type %s"),
    MODIFICATION_NOT_FOUND("modification.notFound", "Modification (%s) not found"),
    MODIFICATIONS_NOT_FOUND("modifications.notFound"),
    MODIFICATION_INFOS_ERROR("modification.infos.error", "Modification infos error : %s"),
    MODIFICATION_WITH_GROUP_DELETION_FORBIDDEN("modification.with_group.deletion.forbidden", "Deletion forbidden : modification %s is owned by group %s"),
    MODIFICATION_DELETION_ARGUMENT_ERROR("modification.deletion.argument.error", "Modification deletion : need to specify the group or give a list of UUIDs"),
    MODIFICATION_DUPLICATION_ARGUMENT_ERROR("modification.duplication.argument.error", "Modification duplication : need to specify the group or give a list of UUIDs"),
    MODIFICATION_DESCRIPTION_MISSING("modification.description.missing"),
    MOVE_COMPOSITE_MODIFICATION_CYCLE_ERROR("modification.composite.move.cycle.error"),
    VOLTAGE_LEVEL_ATTACHMENT_LINE_MISSING("modification.voltageLevel.attachmentLine.missing", "Attachment line for voltage level %s is missing"),
    ROOT_NETWORK_TAG_TOO_LONG("rootNetwork.tag.tooLong", "Root network tag can not be longer than %s characters"),

    NETWORK_NOT_FOUND("network.notFound", "Network %s not found"),
    VARIANT_NOT_FOUND("network.variant.notFound", "Variant %s for network %s not found");

    private final String value;

    private final String messageTemplate;

    ModificationBusinessErrorCode(String value) {
        this.value = value;
        this.messageTemplate = "";
    }

    ModificationBusinessErrorCode(String value, String message) {
        this.value = value;
        this.messageTemplate = message;
    }

    @Override
    public String value() {
        return value;
    }

    public String messageTemplate() {
        return messageTemplate;
    }
}
