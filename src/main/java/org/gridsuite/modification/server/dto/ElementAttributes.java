/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 *
 *     necessary to communicate with directory server
 */
@Getter
@AllArgsConstructor
public class ElementAttributes {
    private UUID uuid;
    private String name;
    private String description;

    public static ElementAttributes createElementAttributes(UUID uuid, String name, String description) {
        return new ElementAttributes(uuid, name, description);
    }
}
