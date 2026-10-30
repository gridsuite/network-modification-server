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
    MODIFICATION_BAD_TYPE("modification.badType", "Modification type of %s is invalid : actual type %s -> expected type %s"),
    MODIFICATION_CONTAINS_SHARED("modification.containsShared", "Modification %s contains a shared modification"),
    MODIFICATIONS_CONTAINS_WRITE_FORBIDDEN_SHARED("modification.sharedWriteForbidden", "The write permission is missing on at least one shared modification of %s"),
    ROOT_NETWORK_TAGS_FORBIDDEN_BY_SHARED("modification.rootNetworkTagsForbiddenByShared", "Root network tags %s are missing on shared modifications %s, which only allow %s"),
    MODIFICATIONS_NOT_FOUND("modifications.notFound"),
    MODIFICATION_INFOS_ERROR("modification.infos.error", "Modification infos error : %s"),
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
